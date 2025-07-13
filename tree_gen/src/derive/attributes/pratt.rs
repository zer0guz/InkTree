use chumsky::{Parser, prelude::any};
use proc_macro2::TokenStream;
use quote::quote;
use snafu::Snafu;
use syn::{Ident, MetaList};

use crate::derive::{
    Language,
    ast::SyntaxVariant,
    attributes::{AttributeError, Inline, LanguageElement, Node},
    codegen::{parseable_impl, struct_def},
    parser::{FromMeta, MetaError},
    properties::{OperatorKind, PrattOperator, PropertyKind},
};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum PrattError {
    // #[snafu(display("todo"))]
    // PathNotIdent {
    //     #[snafu(source(false))]
    //     source: syn::Error,
    // },
}

#[derive(Debug)]
pub struct Pratt {
    pub node: Node,
    operators: Vec<OperatorKind>,
}

impl FromMeta for Pratt {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let input = list.tokens.to_string();
        let node = Node::from_string(input)?;
        Ok(Self {
            node,
            operators: vec![],
        })
    }
}

impl LanguageElement for Pratt {
    fn codegen(
        &self,
        variant: &SyntaxVariant,
        language: &Language,
    ) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &variant.ident);
        let atom = &self.node.parser(&language.idents);
        let operators: &Vec<_> = &language
            .operators
            .iter()
            .map(|op| op.pratt_op(&variant.ident, &language.ident))
            .collect();
        //let parser = quote! {#inner_parser.pratt(#( #operators )*)};
        let parser = quote! {#atom.with_cp().pratt((#( #operators, )*))};
        let impl_code = node_impl(&variant.ident, &language.ident, parser);

        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        use PropertyKind::*;
        &[Root]
    }
}

pub fn node_impl(ident: &Ident, lang_ident: &Ident, body: TokenStream) -> TokenStream {
    let parser = quote! {
        use ::tree_gen::chumsky::prelude::*;
        use ::tree_gen::engine::Builder;
        use ::tree_gen::chumksy_ext::*;
        use ::tree_gen::chumsky::pratt::*;
        #body.wrap_cp(#lang_ident::#ident)
    };
    parseable_impl(parser, ident, lang_ident)
}
