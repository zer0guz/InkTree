use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Ident};

use crate::{
    Error, SyntaxEnum, attributes::SyntaxAttribute, language::element::Element, util::IteratorExt,
};

pub struct Language {
    ident: Ident,
    elements: Vec<Element>,
}

impl Language {
    pub fn from_enum(syntax: SyntaxEnum) -> Result<Self, Error> {
        let elements = syntax
            .variants
            .into_iter()
            .map(|variant| Element::from_variant(variant))
            .collect_either_flatten()?;

        Ok(Self {
            elements,
            ident: syntax.ident,
        })
    }

    pub fn from_input(input: DeriveInput) -> Result<Self, Error> {
        Self::from_enum(SyntaxEnum::from_input(input)?)
    }

    pub fn codegen(&self) -> Result<TokenStream, Error> {
        let mut stream = TokenStream::new();
        let mut static_texts = vec![];
        for element in &self.elements {
            match &element.attribute {
                crate::attributes::SyntaxAttribute::StaticToken(static_token) => {
                    static_texts.push(&static_token.text)
                }
                _ => (),
            }
            element.codegen(&mut stream);
        }
        stream.extend(self.cstree_syntax());
        Ok(stream)
    }

    fn cstree_syntax(&self) -> TokenStream {
        let static_tokens = self
            .elements
            .iter()
            .filter_map(|element| {
                let SyntaxAttribute::StaticToken(static_token) = &element.attribute else {
                    return None;
                };
                let ident = &element.ident;
                let text = &static_token.text;
                Some(quote! {
                    #ident => Some(#text),
                })
            })
            .collect::<Vec<_>>();
        let ident = &self.ident;
        let variant_count = self.elements.len() as u32;
        quote! {
            #[automatically_derived]
            impl tree_gen::cstree::Syntax for #ident {
                fn from_raw(raw: tree_gen::cstree::RawSyntaxKind) -> Self {
                    assert!(raw.0 < #variant_count, "Invalid raw syntax kind: {}", raw.0);
                    // Safety: discriminant is valid by the assert above
                    unsafe { ::std::mem::transmute::<u32, #ident>(raw.0) }
                }

                fn into_raw(self) -> tree_gen::cstree::RawSyntaxKind {
                    tree_gen::cstree::RawSyntaxKind(self as u32)
                }

                fn static_text(self) -> ::core::option::Option<&'static str> {
                    use #ident::*;
                    match self {
                        #( #static_tokens )*
                        _ => None
                    }
                }
            }
        }
    }
}
