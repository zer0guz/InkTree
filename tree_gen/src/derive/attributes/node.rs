use std::collections::{HashMap, HashSet};

use chumsky::{Parser, prelude::just};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use snafu::{ResultExt, Snafu};
use syn::{Ident, LitStr, MetaList};

use crate::derive::{attributes::{AttributeError, LanguageElement}, codegen::{parseable_impl, struct_def}, parser::{dsl_lexer, dsl_parser, DslExpr, FromMeta, MetaError}, properties::PropertyKind};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum NodeError {
    // #[snafu(display("todo"))]
    // PathNotIdent {
    //     #[snafu(source(false))]
    //     source: syn::Error,
    // },
}

#[derive(Debug)]
pub struct Node {
    pub args: DslExpr,
}

impl Node {
    pub fn from_string(input: String) -> Result<Self, NodeError> {
        let tokens = dsl_lexer().parse(input.as_str()).unwrap();
        let args = dsl_parser().parse(&tokens).unwrap();
        Ok(Self { args })
    }

    fn parser(expr: &DslExpr,idents: &HashMap<String,Ident>) -> TokenStream {
        match expr {
            DslExpr::Just(text) => {
                let ident = idents.get(text).unwrap();
                quote! { #ident::parser() }
            }
            DslExpr::Seq(exprs) => {
                exprs
                    .iter()
                    .map(|e| Self::parser(e,idents))
                    .fold(quote! {}, |acc, p| {
                        if acc.is_empty() {
                            p
                        } else {
                            quote! { #acc.then(#p) }
                        }
                    })
            }
            DslExpr::Opt(inner) | DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                let p = Self::parser(inner,idents);
                match expr {
                    &DslExpr::Opt(_) => quote! { #p.or_not() },
                    &DslExpr::Star(_) => quote! { #p.repeated() },
                    &DslExpr::Plus(_) => quote! { #p.repeated().at_least(1) },
                    _ => unreachable!(),
                }
            }
            DslExpr::Alt(a, b) => {
                let pa = Self::parser(a,idents);
                let pb = Self::parser(b,idents);
                quote! { #pa.or(#pb) }
            }
        }
    }
}

impl FromMeta for Node {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let input = list.tokens.to_string();
        Ok(Self::from_string(input)?.into())
    }

}

impl LanguageElement for Node {
    fn codegen(&self, ident: &Ident, lang_ident: &Ident,idents: &HashMap<String,Ident>) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &ident);
        let parser = Self::parser(&self.args,idents);
        let impl_code = node_impl(ident, lang_ident, parser);

        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        &[]
    }
}



pub fn node_impl(ident: &Ident, lang_ident: &Ident,body: TokenStream) -> TokenStream {
    let parser = quote! {       
        use ::tree_gen::chumsky::prelude::*;
        use tree_gen::BuilderParser;
        #body.as_node(#lang_ident::#ident)
    };
    parseable_impl(parser, ident, lang_ident)
}
