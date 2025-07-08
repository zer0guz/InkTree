use std::collections::{HashMap, HashSet};

use enum_dispatch::enum_dispatch;
use proc_macro2::TokenStream;
use quote::quote;
use snafu::{ResultExt, Snafu};
use strum::IntoDiscriminant;
use syn::{DeriveInput, Ident};

use crate::{
    attributes::{
        AttributeError, Node, Root, StaticToken, SyntaxAttribute, SyntaxAttributeKind,
        SyntaxProperty, Token, UnsupportedSnafu, properties::SyntaxPropertyKind,
    },
    error::Errors,
    parser::{EnumError, SyntaxEnum, SyntaxVariant},
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
pub enum LanguageError {
    #[snafu(context(false))]
    Enum { source: EnumError },
    #[snafu(context(false))]
    Attribute { source: AttributeError },
}

pub struct Language {
    ident: Ident,
    root_ident: Option<Ident>,
    pub idents: HashMap<String,Ident>,
    elements: Vec<SyntaxVariant>,
}

impl Language {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            root_ident: None,
            idents: HashMap::new(),
            elements: vec![],
        }
    }

    pub fn from_input(input: DeriveInput) -> Result<Self, Errors<LanguageError>> {
        let syntax_enum = SyntaxEnum::from_input(input).map_err(Errors::map_errors)?;
        let variants = syntax_enum.variants;
        let mut language = Self::new(syntax_enum.ident);

        variants
            .iter()
            .map(|variant| {
                if variant.attribute.discriminant() == SyntaxAttributeKind::Root {
                    language
                        .set_root(variant.ident.clone())
                        .map_err(Errors::from)
                } else {
                    language.idents.insert(variant.ident.to_string(),variant.ident.clone());
                    Ok(())
                }
            })
            .collect_either_flatten()?;

        language.elements = variants;

        if language.root_ident.is_none() {
            todo!("no root error")
        }

        language
            .elements
            .iter()
            .map(|variant| {
                variant
                    .verify()
                    .map_err(Errors::map_errors::<LanguageError>)
            })
            .collect_either_flatten()?;

        Ok(language)
    }

    fn set_root(&mut self, ident: Ident) -> Result<(), LanguageError> {
        let old = self.root_ident.replace(ident);
        if let Some(old) = old
            && let Some(new) = &self.root_ident
        {
            todo!("root error")
        }

        Ok(())
    }
    pub fn codegen(&self) -> Result<TokenStream, Errors<LanguageError>> {
        let mut stream = TokenStream::new();

        stream.extend(self.syntax_impl());

        let code = self
            .elements
            .iter()
            .map(|variant| Ok(variant.codegen(&self.ident,&self.idents)?))
            .collect_either()?;

        stream.extend(code);

        Ok(stream)
    }

    fn syntax_impl(&self) -> TokenStream {
        let static_texts: Vec<_> = self
            .elements
            .iter()
            .map(|variant| {
                let ident = &variant.ident;
                match variant.attribute {
                    SyntaxAttribute::StaticToken(ref static_token) => {
                        let text = &static_token.text;
                        quote! {
                            #ident => Some(#text),
                        }
                    }
                    _ => quote! {
                        #ident => None,
                    },
                }
            })
            .collect();
        let ident = &self.ident;
        let variant_count = self.elements.len() as u32;
        let root = self.root_ident.as_ref().unwrap();
        quote! {
            impl ::tree_gen::cstree::Syntax for #ident {
                fn from_raw(raw: tree_gen::cstree::RawSyntaxKind) -> Self {
                    <Self as ::tree_gen::Syntax>::from_raw(raw)
                }

                fn into_raw(self) -> tree_gen::cstree::RawSyntaxKind {
                    <Self as ::tree_gen::Syntax>::into_raw(self)
                }

                fn static_text(self) -> Option<&'static str> {
                    <Self as ::tree_gen::Syntax>::static_text(self)
                }
            }

            impl ::tree_gen::Syntax for #ident {
                const ROOT: &'static Self = &#ident::#root;

                const STATIC_TOKENS: &'static [Self] = &[TestLang::KwLet, TestLang::KwPub];

                const NODES: &'static [Self] = &[];

                const TOKENS: &'static [Self] = &[];

                fn from_raw(raw: ::tree_gen::cstree::RawSyntaxKind) -> Self {
                    assert!(raw.0 < #variant_count, "Invalid raw syntax kind: {}", raw.0);
                    // Safety: discriminant is valid by the assert above
                    unsafe { ::std::mem::transmute::<u32, #ident>(raw.0) }
                }

                fn into_raw(self) -> ::tree_gen::cstree::RawSyntaxKind {
                    ::tree_gen::cstree::RawSyntaxKind(self as u32)
                }

                fn static_text(self) -> ::core::option::Option<&'static str> {
                    use #ident::*;
                    match self {
                        #( #static_texts )*
                        _ => unreachable!()
                    }
                }
            }
        }
    }
}

#[enum_dispatch]
pub trait LanguageElement: Sized {
    fn codegen(&self, ident: &Ident, lang_ident: &Ident,idents: &HashMap<String,Ident>) -> Result<TokenStream, AttributeError>;
    fn verify(
        &self,
        properties: &Vec<SyntaxProperty>,
        ident: &Ident,
    ) -> Result<(), Errors<AttributeError>> {
        properties
            .iter()
            .map(|prop| {
                self.allowed()
                    .contains(&prop.discriminant())
                    .then_some(prop)
                    .ok_or(syn::Error::new_spanned(ident, "todo text"))
                    .context(UnsupportedSnafu)
            })
            .collect_either()?;

        Ok(())
    }
    fn allowed(&self) -> &'static [SyntaxPropertyKind];
}
