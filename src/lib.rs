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
use quote::quote;
use syn::{parse_macro_input, DeriveInput};
use syn::parse::{Parse, ParseStream};

struct Variant<'a> {
    ident: &'a Ident,
    unit_field_count: usize,
    has_named_fields: bool,
}

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

    let match_arms = generate_match_arms(&variants, &input);

    let enum_ident = &input.ident;
    let generics = &input.generics;

    let count = variants.len();
    let tokens = quote! {
        impl#generics enum_variant_map_types::VariantMap for #enum_ident #generics {
            const COUNT: usize = #count;

            fn ordinal(&self) -> usize {
                match self {
                    #(#match_arms,)*
                }
            }
        }
    };

    tokens.into()
}

fn detect_variants(input: &DeriveInput) -> Vec<Variant> {
    let mut vec = Vec::new();

    let data = match &input.data {
        syn::Data::Enum(data) => data,
        _ => abort_call_site!("cannot derive `Ordinal` on an item which is not an enum"),
    };

    for variant in &data.variants {
        vec.push(detect_variant(variant));
    }

    vec
}

fn detect_variant(variant: &syn::Variant) -> Variant {
    let ident = &variant.ident;

    let (unit_field_count, has_named_fields) = match &variant.fields {
        syn::Fields::Named(_) => (0, true),
        syn::Fields::Unit => (0, false),
        syn::Fields::Unnamed(unnanmed) => (unnanmed.unnamed.len(), false),
    };

    Variant {
        ident,
        unit_field_count,
        has_named_fields,
    }
}

fn generate_match_arms(variants: &[Variant], input: &DeriveInput) -> Vec<TokenStream> {
    let mut vec = Vec::new();
    let enum_ident = &input.ident;

    for (ordinal, variant) in variants.iter().enumerate() {
        let variant_ident = variant.ident;
        let pattern = match (variant.has_named_fields, variant.unit_field_count) {
            (true, _) | (false, 0) => quote! { #enum_ident::#variant_ident { .. } },
            (false, x) => {
                let underscores: Vec<_> = (0..x).map(|_| quote! { _ }).collect();

                quote! {
                    #enum_ident::#variant_ident(#(#underscores),*)
                }
            }
        };

        vec.push(quote! {
            #pattern => #ordinal
        });
    }

    vec
}

#[proc_macro]
pub fn get(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as VariantMapGetter);
}

struct VariantMapGetter {
    map: Ident,
    colon: syn::Path
}

impl Parse for VariantMapGetter {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}
