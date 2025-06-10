mod parser;

use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use crate::parser::{VariantParser, parse_variant};

#[proc_macro_derive(SyntaxGenerator, attributes(tree_gen))]
pub fn derive_my_description(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match my_derive(item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => {
            let compile_error = err.to_compile_error();
            quote!(#compile_error).into()
        }
    }
}

fn my_derive(item: proc_macro2::TokenStream) -> Result<proc_macro2::TokenStream, syn::Error> {
    let mut input = syn::parse2::<syn::DeriveInput>(item)?;
    let mut token_variants = Vec::new();
    let mut node_variant = Vec::new();

    let input_span = input.span();
    let enum_ident = &input.ident;

    if let syn::Data::Enum(syntax) = &mut input.data {
        for variant in &mut syntax.variants {
            match parse_variant(variant)?.parser {
                VariantParser::StaticToken(text) => {
                    let variant_ident = &variant.ident;
                    token_variants.push(quote_spanned!(variant.span()=>
                        #enum_ident :: #variant_ident => Some(#text),
                    ));
                }
                VariantParser::Node => {
                    node_variant.push("");
                }
                VariantParser::None => (),
            }
        }
    }
    let variant_count = token_variants.len() as u32;

    let trait_impl = quote_spanned! { input_span=>
        #[automatically_derived]
        impl ::tree_gen::cstree::Syntax for #enum_ident {
            fn from_raw(raw: ::tree_gen::cstree::RawSyntaxKind) -> Self {
                assert!(raw.0 < #variant_count, "Invalid raw syntax kind: {}", raw.0);
                // Safety: discriminant is valid by the assert above
                unsafe { ::std::mem::transmute::<u32, #enum_ident>(raw.0) }
            }

            fn into_raw(self) -> ::tree_gen::cstree::RawSyntaxKind {
                ::tree_gen::cstree::RawSyntaxKind(self as u32)
            }

            fn static_text(self) -> ::core::option::Option<&'static str> {
                match self {
                    #( #token_variants )*
                    _ => None
                }
            }
        }

        //impl ::tree_gen::StaticToken<E,B> for #enum_ident {}
    };
    Ok(trait_impl)
}

fn to_compile_errors(errors: Vec<syn::Error>) -> proc_macro2::TokenStream {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    quote!(#(#compile_errors)*)
}
