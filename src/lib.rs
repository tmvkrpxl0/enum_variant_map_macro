//! A simple derive macro to generate `ordinal()` and `count()`
//! methods for enums. This is used as index and size for
//! enum_variant_map internally.
//!
//! Unlike `num_derive::ToPrimitive`, this derive macro
//! allows non-C-like enums. The `ordinal` function reflects
//! the variant of the enum and does not account
//! for fields.
//!
//! # Example
//! ```
//! use enum_variant_map_macro::VariantMap;
//! use enum_variant_map_types::VariantMap;
//!
//! #[derive(VariantMap)]
//! enum Animal {
//!     Dog,
//!     Cat {
//!         age: i32,
//!     }
//! }
//!
//! // `ordinal()` ignores assigned values and representation's type
//! #[repr(i32)]
//! #[derive(VariantMap)]
//! enum PrimitiveEnum {
//!     A = 1,
//!     B = 21,
//!     C = 31
//! }
//!
//! assert_eq!(Animal::COUNT, 2);
//! assert_eq!(Animal::Dog.ordinal(), 0);
//! assert_eq!((Animal::Cat { age: 10 }).ordinal(), 1);
//!
//! assert_eq!(PrimitiveEnum::COUNT, 3);
//! assert_eq!(PrimitiveEnum::A.ordinal(), 0usize);
//! assert_eq!(PrimitiveEnum::B.ordinal(), 1usize);
//! assert_eq!(PrimitiveEnum::C.ordinal(), 2usize);
//! ```

use proc_macro2::{Ident, TokenStream};
use proc_macro_error::*;
use quote::{quote, ToTokens};
use syn::{DeriveInput, parse_macro_input, Path, PathArguments, PathSegment, Variant, Visibility};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Comma, PathSep};

/// Generates `fn ordinal(&self) -> usize` and `COUNT` constants for an enum.
///
/// The enum may have any number of variants. It is not
/// required to be a C-like enum, i.e. its variants
/// may have named or unnamed fields.
///
/// The returned ordinals will correspond to the variant's
/// index in the enum definition. For example, the first
/// variant of enum will have ordinal `0`.
#[proc_macro_error]
#[proc_macro_derive(VariantMap)]
pub fn derive_variant_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = detect_variants(&input);
    let visibility = &input.vis;

    let match_arms = generate_match_arms(&variants, &input);
    let getters = generate_ordinal_getters(&variants, visibility);

    let enum_ident = &input.ident;
    let generics = &input.generics;

    let count = variants.len();
    let container_name = Ident::new(&format!("_{}EVMIndexContainer", enum_ident.to_string()), enum_ident.span());
    let tokens = quote! {
        impl#generics enum_variant_map_types::VariantMap for #enum_ident #generics {
            const COUNT: usize = #count;

            fn ordinal(&self) -> usize {
                match self {
                    #(#match_arms,)*
                }
            }
        }

        #visibility struct #container_name;
        impl #container_name {
            #(#getters)*
        }
    };

    tokens.into()
}

fn detect_variants(input: &DeriveInput) -> Vec<&Variant> {
    let mut vec = Vec::new();

    let data = match &input.data {
        syn::Data::Enum(data) => data,
        _ => abort_call_site!("cannot derive `Ordinal` on an item which is not an enum"),
    };

    for variant in &data.variants {
        vec.push(variant);
    }

    vec
}

fn generate_match_arms(variants: &[&Variant], input: &DeriveInput) -> Vec<TokenStream> {
    let mut vec = Vec::new();
    let enum_ident = &input.ident;

    for (ordinal, variant) in variants.iter().enumerate() {
        let variant_ident = &variant.ident;
        let pattern = quote! { #enum_ident::#variant_ident { .. } };
        let arm = quote! {
            #pattern => #ordinal
        };

        vec.push(arm);
    }

    vec
}

fn generate_ordinal_getters(variants: &[&Variant], visibility: &Visibility) -> Vec<TokenStream> {
    let mut getters = vec![];

    for (ordinal, variant) in variants.iter().enumerate() {
        let variant_ident = &variant.ident;
        let function_name = Ident::new(&format!("evm_ordinal_{}", variant_ident.to_string()), variant_ident.span());
        getters.push(quote! {
            #[allow(non_snake_case)]
            #visibility const fn #function_name() -> usize {
                #ordinal
            }
        })
    }

    getters
}

fn new_variant_trait_checker(enum_path: &Punctuated<PathSegment, PathSep>) -> TokenStream {
    quote! {
        let _: Option<enum_variant_map::EnumVariantMap<#enum_path>> = None;
    }
}

fn new_map_type_checker(map_identifier: &Ident, enum_path: &Punctuated<PathSegment, PathSep>) -> TokenStream {
    quote! {
        let _: Option<&#enum_path> = if false {
            #map_identifier.get_by_index(0)
        } else {
            None
        };
    }
}

fn new_variant_name_checker(
    enum_path: &Punctuated<PathSegment, PathSep>,
    enum_path_no_param: &Punctuated<PathSegment, PathSep>,
    variant_ident: &Ident
) -> TokenStream {
    quote! {
        match Option::<#enum_path>::None {
            Some(#enum_path_no_param::#variant_ident { .. }) => {}
            _ => {}
        };
    }
}

#[proc_macro]
pub fn get(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let VariantMapGetter {
        map, enum_path, variant_ident
    } = parse_macro_input!(input as VariantMapGetter);
    let no_path_param = {
        let mut enum_path = enum_path.clone();
        remove_path_parameters(&mut enum_path);
        enum_path
    };

    // checks if supplied enum has VariantMap trait
    let trait_checker = new_variant_trait_checker(&enum_path);

    // checks type of map is correct
    let map_type_checker = new_map_type_checker(&map, &enum_path);

    // checks if the variant name is correct
    let variant_checker = new_variant_name_checker(&enum_path, &no_path_param, &variant_ident);

    let struct_name = {
        format!("_{}EVMIndexContainer", no_path_param.to_token_stream().to_string())
    };
    let function_name = Ident::new(&format!("evm_ordinal_{}", variant_ident), variant_ident.span());
    let container_name = Ident::new(&struct_name, enum_path.span());
    let invoke_getter = quote! {
        unsafe {
            #map.get_by_index_unsafe(#container_name::#function_name())
        }
    };

    let token_stream = quote! {
        {
            #trait_checker
            #map_type_checker
            #variant_checker
            #invoke_getter
        }
    };
    // eprintln!("{}", token_stream);

    token_stream.into()
}

struct VariantMapGetter {
    map: Ident,
    enum_path: Punctuated<PathSegment, PathSep>,
    variant_ident: Ident,
}

impl Parse for VariantMapGetter {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map: Ident = input.parse()?;
        let _: Comma = input.parse()?;
        let path: Path = input.parse()?;

        let segments = &path.segments;
        let (enum_path, variant_ident) = {
            let mut cloned = segments.clone();
            let Some(last) = cloned.pop() else {
                return Err(input.error("Cannot invoke get! without specifying both enum's type and variant's name"));
            };
            cloned.pop_punct();

            let last = last.into_value();
            (cloned, last.ident)
        };

        Ok(Self {
            map,
            enum_path,
            variant_ident,
        })
    }
}

fn remove_path_parameters(path: &mut Punctuated<PathSegment, PathSep>) {
    let Some(last) = path.last() else {
        abort_call_site!("Invalid enum path!")
    } ;
    if let PathArguments::None = last.arguments {
        return;
    }

    let mut last = path.pop().unwrap();
    last.value_mut().arguments = PathArguments::None;
    path.push(last.into_value());
}
