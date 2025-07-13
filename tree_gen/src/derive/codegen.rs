use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use snafu::Snafu;
use syn::{DeriveInput, Ident};

use crate::{
    Errors,
    derive::{
        ast::{EnumError, LanguageEnum, SyntaxVariant},
        attributes::{Attribute, AttributeError},
        properties::Operator,
    },
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
pub enum LanguageError {
    #[snafu(context(false))]
    Enum {
        source: EnumError,
    },
    #[snafu(context(false))]
    Attribute {
        source: AttributeError,
    },
    Root {
        source: syn::Error,
    },
}

pub struct Language {
    pub ident: Ident,
    pub root_ident: Option<Ident>,
    pub idents: HashMap<String, Ident>,
    pub elements: Vec<SyntaxVariant>,
    pub operators: Vec<Operator>,
}

impl Language {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            root_ident: None,
            idents: HashMap::new(),
            elements: vec![],
            operators: vec![],
        }
    }

    pub fn from_input(input: DeriveInput) -> Result<Self, Errors<LanguageError>> {
        let syntax_enum = LanguageEnum::from_input(input).map_err(Errors::map_errors)?;
        let variants = syntax_enum.variants;
        let mut language = Self::new(syntax_enum.ident);

        let elements = variants
            .into_iter()
            .map(|mut element| {
                element
                    .verify(&mut language)
                    .map_err(Errors::map_errors::<LanguageError>)?;
                element
                    .build(&mut language)
                    .map_err(Errors::map_errors::<LanguageError>)?;
                Ok(element)
            })
            .collect_either_flatten()?;

        if language.root_ident.is_none() {
            todo!("no root error")
        }

        language.elements = elements;
        Ok(language)
    }

    pub fn set_root(&mut self, ident: Ident) -> Result<(), LanguageError> {
        let old = self.root_ident.replace(ident);
        if let Some(_) = old
            && let Some(_) = &self.root_ident
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
            .map(|variant| Ok(variant.codegen(&self)?))
            .collect_either()?;

        stream.extend(code);

        Ok(stream)
    }

    fn syntax_impl(&self) -> TokenStream {
        let (static_texts, parsers): (Vec<_>, Vec<_>) = self
            .elements
            .iter()
            .map(|variant| {
                let ident = &variant.ident;
                let static_text = match variant.attribute {
                    Attribute::StaticToken(ref static_token) => {
                        let text = &static_token.text;
                        quote! {
                            #ident => Some(#text),
                        }
                    }
                    _ => quote! {
                        #ident => None,
                    },
                };
                let lang_ident = &self.ident;
                let parser = quote! {
                    #lang_ident::#ident => #ident::parser().boxed(),
                };
                (static_text, parser)
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

                fn parser<'src, 'cache, 'interner, Err>(
                    self,
                ) -> impl ::tree_gen::chumksy_ext::BuilderParser<'src, 'cache, 'interner, (), Err, Self>
                where
                    Err: chumsky::error::Error<'src, ::tree_gen::chumksy_ext::Input<'src>> + 'src,
                    Err: chumsky::label::LabelError<'src,&'src str,chumsky::text::TextExpected<'src,&'src str>>,
                    Err: chumsky::label::LabelError<'src,&'src str,&'src str>,


                    'interner: 'cache,
                    'cache: 'src
                {
                    use ::tree_gen::chumsky::prelude::*;
                    use ::tree_gen::Parseable;
                    match self {
                        #( #parsers )*
                    }
                }
            }
        }
    }
}

pub fn struct_def(body: TokenStream, ident: &Ident) -> TokenStream {
    quote! {
        struct #ident {
            #body
        }
    }
}

pub fn parseable_impl(parser: TokenStream, ident: &Ident, lang_ident: &Ident) -> TokenStream {
    quote! {
        impl ::tree_gen::Parseable for #ident {
            type Syntax = TestLang;

            fn parser<'src, 'cache, 'interner, Err>()
            -> impl ::tree_gen::chumksy_ext::BuilderParser<'src, 'cache, 'interner, (), Err, #lang_ident>
            where
                Err: chumsky::error::Error<'src, &'src str> + 'src,
                Err: chumsky::label::LabelError<'src,&'src str,chumsky::text::TextExpected<'src,&'src str>>,
                Err: chumsky::label::LabelError<'src,&'src str,&'src str>,


                'cache: 'src,
                'interner: 'cache,
            {
                #parser
            }
        }
    }
}
