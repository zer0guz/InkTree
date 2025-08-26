use crate::derive::language::language_element::LanguageElement;
use std::
    collections::{HashMap, hash_map::Entry}
;

use proc_macro2::TokenStream;
use snafu::{ResultExt, Snafu};
use syn::{DeriveInput, Ident, parse::Parse};

use quote::quote;

use crate::{
    derive::{
        attributes::SyntaxAttribute,
        language::{ElementError, SyntaxElement},
        properties::Operator,
    },
    error::Errors,
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
pub enum LanguageError {
    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    Data { source: syn::Error },

    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    Repr { source: syn::Error },

    #[snafu(context(false))]
    Custom { source: syn::Error },

    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    #[snafu(context(false))]
    Element { source: ElementError },
}

pub struct Language {
    pub elements: Vec<SyntaxElement>,
    pub ident: Ident,
    pub operators: Vec<Operator>,
    pub root_idents: Vec<Ident>,
    pub idents: HashMap<String, Ident>,
}

impl Language {
    pub fn from_input(input: DeriveInput) -> Result<Self, Errors<LanguageError>> {
        let mut language = Self {
            elements: Vec::with_capacity(input.attrs.len()),
            ident: input.ident.clone(),
            idents: HashMap::new(),
            operators: vec![],
            root_idents: vec![],
        };

        let mut repr = vec![];

        input
            .attrs
            .iter()
            .filter(|attr| {
                if attr.path().is_ident("repr") {
                    repr.push(*attr);
                }
                attr.path().is_ident("tree_gen")
            })
            .map(|attribute| {
                language.handle_element(
                    SyntaxElement::from_attribute(&attribute).map_err(LanguageError::from)?,
                )
            })
            .collect_either_flatten()?;

        let repr_inner = repr
            .into_iter()
            .next()
            .ok_or_else(|| syn::Error::new_spanned(&input, "todo text repr"))
            .context(ReprSnafu)?
            .parse_args_with(Ident::parse)
            .context(ReprSnafu)?;

        if !(repr_inner == "u32") {
            return Err(syn::Error::new_spanned(repr_inner, "todo text repr2"))
                .context(ReprSnafu)?;
        };

        let syn::Data::Enum(syntax) = &input.data else {
            Err(syn::Error::new_spanned(&input, "oups")).context(DataSnafu)?
        };

        syntax
            .variants
            .iter()
            .map(SyntaxElement::from_variant)
            .collect_either_flatten_into()?
            .into_iter()
            .flatten()
            .map(|element| language.handle_element(element))
            .collect_either_flatten()?;

        Ok(language)
    }

    fn handle_element(&mut self, element: SyntaxElement) -> Result<(), Errors<LanguageError>> {
        let name = element.attribute.name();

        match self.idents.entry(name.to_string()) {
            Entry::Vacant(v) => {
                v.insert(name.clone());
            }
            Entry::Occupied(_) => {
                return Err(syn::Error::new_spanned(name, "duplicate ident"))
                    .map_err(Errors::from)
                    .map_err(Errors::map_errors);
            }
        }

        element.build(self).map_err(Errors::map_errors)?;

        self.elements.push(element);

        return Ok(());
    }

    pub fn codegen(self) -> Result<TokenStream, Errors<LanguageError>> {
        match self.root_idents.len() {
            0 => Err(syn::Error::new_spanned(&self.ident, "no root todo text")),
            1 => Ok(()),
            _ => Err(syn::Error::new_spanned(
                &self.root_idents[1],
                "multiple roots todo text",
            )),
        }
        .map_err(LanguageError::from)?;

        let mut stream = TokenStream::new();

        stream.extend(self.syntax_impl());

        let variants_code = self
            .elements
            .iter()
            .map(|variant| Ok(variant.codegen(&self)?))
            .collect_either()?;

        stream.extend(variants_code);

        Ok(stream)
    }

    fn syntax_impl(&self) -> TokenStream {
        let (static_texts, parsers): (Vec<_>, Vec<_>) = self
            .elements
            .iter()
            .map(|variant| {
                let ident = &variant.attribute.name();
                let static_text = match variant.attribute {
                    SyntaxAttribute::StaticToken(ref static_token) => {
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
        let root = &self.root_idents[0];
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

                const STATIC_TOKENS: &'static [Self] = &[];

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

                'cache: 'src,
                'interner: 'cache,
            {
                #parser
            }
        }
    }
}
