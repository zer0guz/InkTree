use crate::{
    derive::attributes::SyntaxAttributeKind,
    language::rule_graph::{RecursionInfo, RuleGraph},
    util::{Handle, Pool},
};
use std::collections::HashSet;

use proc_macro2::TokenStream;
use snafu::{ResultExt, Snafu};
use strum::IntoDiscriminant;
use syn::{DeriveInput, Ident, parse::Parse};

use quote::quote;

use crate::{
    derive::{
        attributes::SyntaxAttribute,
        language::{Element, ElementError, LanguageElement},
        properties::Operator,
    },
    error::Errors,
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
pub enum Error {
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

pub(crate) struct Language {
    pub elements: Pool<Element>,
    pub ident: Ident,
    pub operators: Vec<Operator>,
    pub root_idents: Vec<Ident>,
    pub idents: HashSet<Ident>,
    pub cycle_graph: RuleGraph,
    pub recursion_info: Option<RecursionInfo>,
    pub rules: Vec<Handle<Element>>,
}

impl Language {
    fn handle_element(&mut self, element: Element) -> Result<(), Errors<Error>> {
        element.build(self).map_err(Errors::map_errors)?;
        self.elements.push(element);

        return Ok(());
    }

    fn syntax_impl(&self) -> TokenStream {
        let (static_texts, parsers): (Vec<_>, Vec<_>) = self
            .elements
            .iter()
            .filter(|element| element.attribute.discriminant() != SyntaxAttributeKind::Rule)
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
                ) -> impl ::tree_gen::chumsky_ext::BuilderParser<'src, 'cache, 'interner, (), Err, Self> + Clone
                where
                    Err: chumsky::error::Error<'src, ::tree_gen::chumsky_ext::Input<'src>> + 'src,

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

pub fn build(input: DeriveInput) -> Result<TokenStream, Errors<Error>> {
    let mut language = Language {
        elements: Pool::with_capacity(input.attrs.len() as u32),
        ident: input.ident.clone(),
        idents: HashSet::new(),
        operators: vec![],
        root_idents: vec![],
        cycle_graph: RuleGraph::new(),
        recursion_info: None,
        rules: vec![],
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
            language.handle_element(Element::from_attribute(&attribute).map_err(Error::from)?)
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
        return Err(syn::Error::new_spanned(repr_inner, "todo text repr2")).context(ReprSnafu)?;
    };

    let syn::Data::Enum(syntax) = &input.data else {
        Err(syn::Error::new_spanned(&input, "oups")).context(DataSnafu)?
    };

    syntax
        .variants
        .iter()
        .map(Element::from_variant)
        .collect_either_flatten_into()?
        .into_iter()
        .flatten()
        .map(|element| language.handle_element(element))
        .collect_either_flatten()?;

    let rules = language
        .rules
        .iter()
        .map(|handle| {
            let element = &language.elements[*handle];
            let rule = element.rule().expect("comes from rules");
            (rule.name.clone(), rule)
        })
        .collect();

    language.recursion_info = Some(language.cycle_graph.into_recursive_info(&rules));

    if language
        .recursion_info
        .as_ref()
        .expect("bug")
        .left_recursive
        .len()
        > 0
    {
        todo!("REEEE FIX YOUR LEFT RECURSION")
    }

    match language.root_idents.len() {
        0 => Err(syn::Error::new_spanned(
            &language.ident,
            "no root todo text",
        )),
        1 => Ok(()),
        _ => Err(syn::Error::new_spanned(
            &language.root_idents[1],
            "multiple roots todo text",
        )),
    }
    .map_err(Error::from)?;

    let mut stream = quote! {
        use tree_gen::Parseable;
    };

    stream.extend(language.syntax_impl());

    let variants_code = language
        .elements
        .iter()
        .map(|variant| Ok(variant.codegen(&language)?))
        .collect_either()?;

    stream.extend(variants_code);

    Ok(stream)
}
