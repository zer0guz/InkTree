use std::collections::HashMap;

use chumsky::Parser;
use derive_more::From;
use proc_macro2::TokenStream;
use quote::quote;
use snafu::Snafu;
use syn::{Ident, MetaList};

use crate::derive::{
    Language,
    ast::SyntaxVariant,
    attributes::{AttributeError, LanguageElement},
    codegen::{parseable_impl, struct_def},
    parser::{DslExpr, FromMeta, MetaError, dsl_lexer, dsl_parser},
    properties::PropertyKind,
};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum NodeError {
    // #[snafu(display("todo"))]
    // PathNotIdent {
    //     #[snafu(source(false))]
    //     source: syn::Error,
    // },
}

#[derive(Debug, From)]
pub struct Inline(DslExpr);

impl Inline {
    pub fn from_string(input: String) -> Result<Self, NodeError> {
        let tokens = dsl_lexer().parse(input.as_str()).unwrap();
        let args = dsl_parser().parse(&tokens).unwrap();
        Ok(Self(args.into()))
    }

    pub fn parser(expr: &DslExpr, idents: &HashMap<String, Ident>) -> TokenStream {
        match expr {
            DslExpr::Just(text) => {
                let ident = idents.get(text).unwrap();
                quote! { #ident::parser() }
            }
            DslExpr::Seq(exprs) => {
                exprs
                    .iter()
                    .map(|e| Self::parser(e, idents))
                    .fold(quote! {}, |acc, p| {
                        if acc.is_empty() {
                            p
                        } else {
                            quote! { #acc.then(#p) }
                        }
                    })
            }
            DslExpr::Opt(inner) | DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                let p = Self::parser(inner, idents);
                match expr {
                    &DslExpr::Opt(_) => quote! { #p.or_not() },
                    &DslExpr::Star(_) => quote! { #p.repeated() },
                    &DslExpr::Plus(_) => quote! { #p.repeated().at_least(1) },
                    _ => unreachable!(),
                }
            }
            DslExpr::Alt(a, b) => {
                let pa = Self::parser(a, idents);
                let pb = Self::parser(b, idents);
                quote! { #pa.or(#pb) }
            }
        }
    }
}

#[derive(Debug, From)]
pub struct Node(Inline);

impl Node {
    pub fn parser(&self, idents: &HashMap<String, Ident>) -> TokenStream {
        Inline::parser(&self.0.0, idents)
    }
    pub fn from_string(input: String) -> Result<Self, NodeError> {
        Ok(Inline::from_string(input)?.into())
    }
}

impl FromMeta for Inline {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let input = list.tokens.to_string();
        Ok(Self::from_string(input)?.into())
    }
}

impl FromMeta for Node {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let input = list.tokens.to_string();
        Ok(Self::from_string(input)?.into())
    }
}

impl LanguageElement for Inline {
    fn codegen(
        &self,
        variant: &SyntaxVariant,
        language: &Language,
    ) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &variant.ident);
        let parser = Inline::parser(&self.0, &language.idents);
        let impl_code = node_impl(&variant.ident, &language.ident, parser, false);
        Ok(quote! {
            #def
            #impl_code
        })
    }
    fn allowed(&self) -> &'static [PropertyKind] {
        &[PropertyKind::Padded, PropertyKind::PaddedBy]
    }
}

impl LanguageElement for Node {
    fn codegen(
        &self,
        variant: &SyntaxVariant,
        language: &Language,
    ) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &variant.ident);
        let parser = self.parser(&language.idents);
        let impl_code = node_impl(&variant.ident, &language.ident, parser, true);
        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        &[
            PropertyKind::Root,
            PropertyKind::Padded,
            PropertyKind::PaddedBy,
        ]
    }
}

pub fn node_impl(
    ident: &Ident,
    lang_ident: &Ident,
    body: TokenStream,
    as_node: bool,
) -> TokenStream {
    let mut parser = quote! {
        use ::tree_gen::chumsky::prelude::*;
        use tree_gen::chumksy_ext::*;
        #body
    };
    if as_node {
        parser = quote! {#parser.as_node(#lang_ident::#ident)}
    }
    parseable_impl(parser, ident, lang_ident)
}
