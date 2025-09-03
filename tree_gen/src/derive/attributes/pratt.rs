use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, MetaList};

use crate::{
    derive::{
        attributes::{Node, allowed::ALLOWED_PRATT},
        parser::FromMeta,
        properties::{Operator, Property, PropertyKind},
    },
    language::{ElementError, Language, LanguageElement},
};

#[derive(Debug)]
pub struct Pratt {
    pub node: Node,
}

impl Pratt {
    pub fn code(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;
        let name_ident = &self.name();

        // base body from Rule
        let base_body = self.node.0.parser_body(language);

        let operators = &language.operators;
        let pratt_ops = Self::pratt_table(&self, lang_ident, operators);

        let wrapped_body = quote! {
            #base_body.with_cp().pratt(#pratt_ops)
        };

        let final_body = quote! {
            use ::tree_gen::chumsky::prelude::*;
            use ::tree_gen::engine::Builder;
            use ::tree_gen::chumsky_ext::*;
            use ::tree_gen::chumsky::pratt::*;
            #wrapped_body.boxed().wrap_cp(#lang_ident::#name_ident)
        };

        // delegate to Rule’s impl wrapper
        self.node.0.parser(final_body, language, true)
    }

    fn pratt_table(&self, lang_ident: &Ident, operators: &Vec<Operator>) -> TokenStream {
        let operators = operators
            .iter()
            .map(|operator| operator.pratt_op(self.name(), lang_ident));

        quote! {
            //fn pratt_table() -> &'static [PrattOp<Self>] {
                (#( #operators ),* )
            //}
        }
    }
}

impl FromMeta for Pratt {
    fn from_list(list: &MetaList, name: Option<&Ident>) -> Result<Self, ElementError> {
        let input = list.tokens.to_string();
        let node = Node::from_string(input, name.expect("asdasd"))?;
        Ok(Self { node })
    }
}

impl LanguageElement for Pratt {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {
        let code = self.code(language);
        Ok(quote! {
            #code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_PRATT
    }

    fn name(&self) -> &Ident {
        self.node.name()
    }

    fn build(
        &self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        self.node.build(properties, language)
    }
}
