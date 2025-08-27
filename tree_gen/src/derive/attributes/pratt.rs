use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, MetaList};

use crate::{
    derive::{
        attributes::{Node, allowed::ALLOWED_PRATT},
        language::{parseable_impl, struct_def},
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
        let name = &self.name();

        let operators = &language.operators;

        let atom_body = &self.node.parser_body(language);

        let pratt_ops = Self::pratt_table(&self, lang_ident, operators);

        let body = quote! {#atom_body.with_cp().pratt(#pratt_ops)};

        let parser = quote! {
            use ::tree_gen::chumsky::prelude::*;
            use ::tree_gen::engine::Builder;
            use ::tree_gen::chumksy_ext::*;
            use ::tree_gen::chumsky::pratt::*;
            #body.boxed().wrap_cp(#lang_ident::#name)
        };

        let parseable_impl = parseable_impl(parser, name, lang_ident);

        quote! {
            #parseable_impl
        }
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
        let def_body = quote! {};
        let struct_def = struct_def(def_body, &self.name());

        let impls = self.code(language);
        Ok(quote! {
            #struct_def
            #impls
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_PRATT
    }

    fn name(&self) -> &Ident {
        self.node.name()
    }

    fn build(
        &mut self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        self.node.build(properties, language)?;
        Ok(())
    }
}
