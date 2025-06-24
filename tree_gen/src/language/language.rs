use proc_macro2::TokenStream;
use syn::DeriveInput;

use crate::{language::element::Element, util::IteratorExt, Error, SyntaxEnum};

pub struct Language {
    elements: Vec<Element>,
}

impl Language {
    pub fn from_enum(syntax: SyntaxEnum) -> Result<Self, Error> {
        let elements = syntax
            .variants
            .into_iter()
            .map(|variant| Element::from_variant(variant))
            .collect_either_flatten()?;

        Ok(Self { elements })
    }

    pub fn from_input(input: DeriveInput) -> Result<Self, Error> {
        Self::from_enum(SyntaxEnum::from_input(input)?)
    }

    pub fn codegen(&self) -> Result<TokenStream, Error> {
        let mut stream = TokenStream::new();
        for element in &self.elements {
             element.codegen(&mut stream);
        }
        Ok(stream)
    }
}

// pub fn cstree_syntax(&self) -> Result<TokenStream, Vec<Error>> {
//     let trait_impl = quote_spanned! { syntax_kind_enum.source.span()=>
//         #[automatically_derived]
//         impl ::cstree::Syntax for #name {
//             fn from_raw(raw: ::cstree::RawSyntaxKind) -> Self {
//                 assert!(raw.0 < #variant_count, "Invalid raw syntax kind: {}", raw.0);
//                 // Safety: discriminant is valid by the assert above
//                 unsafe { ::std::mem::transmute::<u32, #name>(raw.0) }
//             }

//             fn into_raw(self) -> ::cstree::RawSyntaxKind {
//                 ::cstree::RawSyntaxKind(self as u32)
//             }

//             fn static_text(self) -> ::core::option::Option<&'static str> {
//                 match self {
//                     #( #static_texts )*
//                 }
//             }
//         }
//     };
// }
