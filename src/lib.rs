//! # `toml-cfg`
//!
//! ## Summary
//!
//! * Crates can declare variables that can be overridden
//!     * Anything const, e.g. usize, strings, etc.
//! * (Only) The "root crate" can override these variables by including a `cfg.toml` file
//!
//! ## Config file
//!
//! This is defined ONLY in the final application or "root crate"
//!
//! ```toml
//! # a toml-cfg file
//!
//! [lib-one]
//! buffer_size = 4096
//!
//! [lib-two]
//! greeting = "Guten tag!"
//! ```
//!
//! ## In the library
//!
//! ```rust
//! // lib-one
//! #[derive(toml_cfg::TomlConfig)]
//! pub struct Config {
//!     #[toml_cfg(default(32))]
//!     buffer_size: usize,
//! }
//! ```
//!
//!```rust
//! // lib-two
//! #[derive(toml_cfg::TomlConfig)]
//! pub struct Config {
//!     #[toml_cfg(default("hello"))]
//!     greeting: &'static str,
//! }
//!
//! ```
//!
//! ## Configuration
//!
//! With the `TOML_CFG` environment variable is set with a value containing
//! "require_cfg_present", the `toml-cfg` proc macro will panic if no valid config
//! file is found. This is indicative of either no `cfg.toml` file existing in the
//! "root project" path, or a failure to find the correct "root project" path.
//!
//! This failure could occur when NOT building with a typical `cargo build`
//! environment, including with `rust-analyzer`. This is *mostly* okay, as
//! it doesn't seem that Rust Analyzer presents this in some misleading way.
//!
//! If you *do* find a case where this occurs, please open an issue!
//!
//! ## Look at what we get!
//!
//! ```shell
//! # Print the "buffer_size" value from the `lib-one` crate.
//! # Since it has no cfg.toml, we just get the default value.
//! $ cd pkg-example/lib-one
//! $ cargo run
//!     Finished dev [unoptimized + debuginfo] target(s) in 0.01s
//!      Running `target/debug/lib-one`
//! 32
//!
//! # Print the "greeting" value from the `lib-two` crate.
//! # Since it has no cfg.toml, we just get the default value.
//! $ cd ../lib-two
//! $ cargo run
//!    Compiling lib-two v0.1.0 (/home/james/personal/toml-cfg/pkg-example/lib-two)
//!     Finished dev [unoptimized + debuginfo] target(s) in 0.32s
//!      Running `target/debug/lib-two`
//! hello
//!
//! # Print the "buffer_size" value from `lib-one`, and "greeting"
//! # from `lib-two`. Since we HAVE defined a `cfg.toml` file, the
//! # values defined there are used instead.
//! $ cd ../application
//! $ cargo run
//!    Compiling lib-two v0.1.0 (/home/james/personal/toml-cfg/pkg-example/lib-two)
//!    Compiling application v0.1.0 (/home/james/personal/toml-cfg/pkg-example/application)
//!     Finished dev [unoptimized + debuginfo] target(s) in 0.30s
//!      Running `target/debug/application`
//! 4096
//! Guten tag!
//! ```
//!

use heck::{ToShoutySnakeCase, ToSnakeCase};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_warning::Warning;
use quote::{quote, ToTokens};
use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use syn::parse;
use syn::spanned::Spanned;
use syn::token::Token;
use syn::Expr;

type Crates = HashMap<String, toml::Table>;
type Parsed = Vec<(Vec<syn::Ident>, syn::Expr)>;

struct FieldConfig {
    default: (Expr, bool), // true if deprecated
}

impl parse::Parse for FieldConfig {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let mut default = None;
        let mut deprecated = Vec::new();

        while !input.is_empty() {
            let lookahead = input.lookahead1();

            if parse_paren(&mut default, &lookahead, input, tokens::default)? {
            } else if input.peek(tokens::deprecated) {
                input.parse::<tokens::deprecated>()?;

                let content;
                syn::parenthesized!(content in input);

                if content.parse::<tokens::default>().is_ok() {
                    deprecated.push(tokens::default::display());
                }

                if content.peek(syn::Token![,]) {
                    content.parse::<syn::Token![,]>()?;
                }
            } else {
                return Err(lookahead.error());
            }
        }

        let default = {
            let is_deprecated = deprecated.contains(&tokens::default::display());
            let Some(value) = default else {
                let message = format!("Attribute {} is not present", tokens::default::display());
                return Err(syn::Error::new(Span::call_site(), message));
            };
            (value, is_deprecated)
        };

        Ok(Self { default })
    }
}

struct Rettrigerring(PathBuf);
impl ToTokens for Rettrigerring {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let cfg_path = format!("{}", self.0.display());
        tokens.extend(quote! {
            const _: &[u8] = include_bytes!(#cfg_path);
        })
    }
}

struct Config {
    input: syn::ItemStruct,
    fields_config: Vec<FieldConfig>,
    retrigger: Option<Rettrigerring>,
    config: Parsed,
    is_attribute: bool,
}

impl Config {
    const FILE_NAME: &'static str = "cfg.toml";
    const ENV_KEY: &'static str = "TOML_CFG";
    const ENV_VALUE: &'static str = "require_cfg_present";

    fn new(input: TokenStream, is_attribute: bool) -> syn::Result<Self> {
        let mut input = syn::parse::<syn::ItemStruct>(input).map_err(|esyn| {
            const MESSAGE: &'static str = "Failed to parse configuration structure!";

            let mut error = syn::Error::new(esyn.span(), MESSAGE);
            error.combine(esyn);

            error
        })?;

        let fields_config = Self::fields_config(&mut input)?;

        let path = Self::get_path();
        let config = path.as_ref().and_then(|path| Self::load(input.span(), path)).transpose()?;

        let retrigger = path.filter(|_| config.is_some()).map(Rettrigerring);
        let config = Self::apply_config(&input, config.unwrap_or_default())?;

        Ok(Self { input, fields_config, retrigger, config, is_attribute })
    }

    fn fields_config(input: &mut syn::ItemStruct) -> syn::Result<Vec<FieldConfig>> {
        let mut configs = Vec::with_capacity(input.fields.len());

        for field in input.fields.iter_mut() {
            if field.ident.is_none() {
                const MESSAGE: &'static str =
                    "Failed to find field identifier. Don't use this on a tuple struct";
                return Err(syn::Error::new(field.span(), MESSAGE));
            }

            let attrs = {
                let attrs = core::mem::take(&mut field.attrs);
                let (our, other) = attrs.into_iter().partition(|attr| {
                    let path = attr.path();
                    path.is_ident("toml_cfg") || path.is_ident("default") // default added for backwards compatibility
                });
                field.attrs = other;
                our
            };

            let tokens = attrs.into_iter().try_fold(TokenStream2::default(), |mut ts, attr| {
                let mut content = attr.parse_args::<TokenStream2>()?;

                if attr.path().is_ident("default") {
                    let default = tokens::default::default();
                    content = quote! { #default (#content) };

                    let deprecated = tokens::deprecated::default();
                    ts.extend(quote! { #deprecated (#default)});
                }

                ts.extend(content);

                syn::Result::Ok(ts)
            })?;
            configs.push(syn::parse2::<FieldConfig>(tokens)?);
        }

        Ok(configs)
    }

    fn apply_config(input: &syn::ItemStruct, config: toml::Table) -> syn::Result<Parsed> {
        let path = vec![Self::generate_inner_ident(&input.ident)];
        let value = toml::Value::Table(config);
        let mut parsed = Vec::new();

        Self::apply_config_inner(path, &value, &mut parsed)?;
        Ok(parsed)
    }

    fn apply_config_inner(
        path: Vec<syn::Ident>,
        value: &toml::Value,
        configs: &mut Parsed,
    ) -> syn::Result<()> {
        if let Some(table) = value.as_table() {
            for (field, value) in table {
                let mut path = path.clone();
                path.push(syn::parse_str::<syn::Ident>(field)?);
                Self::apply_config_inner(path, value, configs)?;
            }
        } else {
            let t_string = value.to_string();
            let value = syn::parse_str::<Expr>(&t_string).map_err(|mut err| {
                let message =
                    format!("Failed to parse `{value}` as a valid token!", value = &t_string);
                err.combine(syn::Error::new(err.span(), message));
                err
            })?;
            configs.push((path, value));
        }
        Ok(())
    }

    fn load(span: Span, path: impl AsRef<Path>) -> Option<syn::Result<toml::Table>> {
        // Load toml data
        let mut crates = {
            let contents = std::fs::read_to_string(path).ok()?;
            toml::from_str::<Crates>(&contents).ok()?
        };

        // Take config for current crate
        let pkg_name = env::var("CARGO_PKG_NAME").ok()?;
        let config = crates.remove(&pkg_name);

        // If the config is not found, but it is required
        if config.is_none() && Self::is_require_present() {
            let message = format!(
                "{key}={value} set, but valid config not found!",
                key = Self::ENV_KEY,
                value = Self::ENV_VALUE
            );
            return Some(Err(syn::Error::new(span, message)));
        }

        config.map(Ok)
    }

    fn is_require_present() -> bool {
        env::var(Self::ENV_KEY).is_ok_and(|value| value.contains(Self::ENV_VALUE))
    }

    fn get_path() -> Option<PathBuf> {
        let mut path = find_root_path()?;
        path.push(Self::FILE_NAME);
        Some(path)
    }

    fn generate_inner_ident(ident: &syn::Ident) -> syn::Ident {
        syn::Ident::new(&ident.to_string().to_snake_case(), ident.span())
    }

    fn generate_default(&self, warnings: &mut Vec<Warning>) -> TokenStream2 {
        let fields = self.input.fields.iter().zip(self.fields_config.iter());
        let fields = fields.fold(vec![], |mut out, (field, config)| {
            let (value, is_deprecated) = &config.default;

            if let Some(warning) = is_deprecated.then(|| {
                let title = field.ident.as_ref().unwrap();
                let value = value.to_token_stream().to_string();
                Warning::new_deprecated(format!("{title}_default"))
                    .old(format!("#[default({value})]"))
                    .new(format!("#[toml_cfg(default({value}))]"))
                    .span(field.span())
                    .build_or_panic()
            }) {
                warnings.push(warning);
            }

            out.push({
                let ident = field.ident.as_ref();
                quote! { #ident: #value }
            });

            out
        });

        let ident = &self.input.ident;
        quote! {
            #ident {
                #(#fields),*
            }
        }
    }
}

impl ToTokens for Config {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ident = &self.input.ident;
        let mut warnings = Vec::new();

        if self.is_attribute {
            let warning = Warning::new_deprecated("_attribute_toml_config")
                .old("#[toml_cfg::toml_config]")
                .new("#[derive(toml_cfg::TomlConfig)]")
                .build_or_panic();
            warnings.push(warning);

            let input = &self.input;
            tokens.extend(quote::quote_spanned! { input.span() => #input  })
        }

        let inner = Self::generate_inner_ident(ident);
        let outer = syn::Ident::new(&ident.to_string().to_shouty_snake_case(), ident.span());
        let default = self.generate_default(&mut warnings);

        let config = self.config.iter().filter_map(|(field, value)| {
            let config = field.get(1)?;
            (self.input.fields.iter().any(|f| f.ident.as_ref().is_some_and(|i| i == config)))
                .then(|| quote! { #(#field).* = #value })
        });

        tokens.extend(quote! {
            pub const #outer: #ident = {
                let mut #inner = #default;
                {
                    #(#config);*
                }
                #inner
            };
        });

        let retrigger = &self.retrigger;
        let private = syn::Ident::new(
            &format!("_toml_cfg_{}", ident.to_string().to_snake_case()),
            ident.span(),
        );
        tokens.extend(quote! {
            mod #private {
                #retrigger

                #(#warnings)*
            }
        })
    }
}

mod tokens {
    syn::custom_keyword!(deprecated);
    syn::custom_keyword!(default);
}

fn find_root_path() -> Option<PathBuf> {
    // First we get the arguments for the rustc invocation
    let mut args = std::env::args().peekable();

    // Then we loop through them all, and find the value of "out-dir"
    let mut out_dir = None;
    while let Some(arg) = args.next() {
        if arg == "--extern" && args.peek().is_some_and(|arg| arg.starts_with("toml_cfg")) {
            out_dir = args.next();
            break;
        }
    }

    // Finally we clean out_dir by removing all trailing directories, until it ends with target
    let mut out_dir = PathBuf::from(out_dir?.trim_start_matches("toml_cfg="));
    while !out_dir.ends_with("target") {
        if !out_dir.pop() {
            // We ran out of directories...
            return None;
        }
    }

    out_dir.pop();

    Some(out_dir)
}

#[proc_macro_attribute]
pub fn toml_config(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let out = match Config::new(item, true) {
        Ok(config) => config.into_token_stream(),
        Err(err) => err.into_compile_error(),
    };
    out.into()
}

#[proc_macro_derive(TomlConfig, attributes(toml_cfg))]
pub fn toml_config_derive(item: TokenStream) -> TokenStream {
    let out = match Config::new(item, false) {
        Ok(config) => config.into_token_stream(),
        Err(err) => err.into_compile_error(),
    };
    out.into()
}

fn parse_paren<T: parse::Peek, R: parse::Parse>(
    ret: &mut Option<R>,
    lookaheed: &parse::Lookahead1,
    input: &parse::ParseBuffer,
    token: T,
) -> syn::Result<bool>
where
    T::Token: parse::Parse + Spanned,
{
    if lookaheed.peek(token) {
        let token = input.parse::<T::Token>()?;

        let content;
        syn::parenthesized!(content in input);

        if ret.is_some() {
            let message = format!("Duplicate {} attribute", T::Token::display());
            return Err(syn::Error::new(token.span(), message));
        }
        *ret = Some(content.parse()?);

        if input.peek(syn::Token![,]) {
            input.parse::<syn::Token![,]>()?;
        }
        Ok(true)
    } else {
        Ok(false)
    }
}
