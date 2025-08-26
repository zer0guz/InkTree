use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, MetaList};

use crate::{derive::{
    attributes::Node, language::{parseable_impl, struct_def}, parser::{FromMeta}, properties::PropertyKind
}, language::{ElementError, Language, LanguageElement}};

#[derive(Debug)]
pub struct Pratt {
    pub node: Node,
}

impl Pratt {
    pub fn parser(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;
        let ident = &self.name();

        let operators: &Vec<_> = &language
            .operators
            .iter()
            .map(|op| op.pratt_op(&self.name(), &language.ident))
            .collect();

        let atom_body = &self.node.parser_body(language);

        let body = quote! {#atom_body.with_cp().pratt((#( #operators, )*))};

        let parser = quote! {
            use ::tree_gen::chumsky::prelude::*;
            use ::tree_gen::engine::Builder;
            use ::tree_gen::chumksy_ext::*;
            use ::tree_gen::chumsky::pratt::*;
            #body.wrap_cp(#lang_ident::#ident)
        };
        parseable_impl(parser, ident, lang_ident)
    }
}

impl FromMeta for Pratt {
    fn from_list(list: &MetaList,name:Option<&Ident>) -> Result<Self, ElementError> {
        let input = list.tokens.to_string();
        let node = Node::from_string(input,name.expect("asdasd"))?;
        Ok(Self { node })
    }
}

impl LanguageElement for Pratt {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {

        let def_body = quote! {};
        let def = struct_def(def_body, &self.name());

        let code = self.parser( language);
        Ok(quote! {
            #def
            #code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        use PropertyKind::*;
        &[Root]
    }
    
    fn name(&self) ->  &Ident {
        self.node.name()
    }
}
