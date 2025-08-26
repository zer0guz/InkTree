
use std::collections::HashMap;

use chumsky::Parser;
use derive_more::From;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, MetaList};

use crate::{
    derive::{
        attributes::{
            allowed::ALLOWED_NODE, rule::Rule
        }, language::{parseable_impl, struct_def}, parser::{dsl_lexer, dsl_parser, FromMeta}, properties::PropertyKind
    }, language::{ElementError, Language, LanguageElement}}
;

#[derive(Debug, From)]
pub struct Node(Rule);

impl Node {
    pub fn from_string(input: String, name: &Ident) -> Result<Self, syn::Error> {
        let tokens = dsl_lexer().parse(&input).into_result().expect("lexer error todo");
        let dsl = dsl_parser().parse(&tokens).into_result().expect("parser error todo").into();
        Ok(Self(Rule{dsl,name: name.clone(), parameters: HashMap::new()}))
    }

    pub fn parser(&self, language: &Language) -> TokenStream {
        let body = self.0.parser_body(language);
        let lang_ident = &language.ident;
        let name = &self.name();

        let mut parser = quote! {
            use ::tree_gen::chumsky::prelude::*;
            use tree_gen::chumksy_ext::*;
            #body
        };
        parser = quote! {#parser.as_node(#lang_ident::#name)};

        parseable_impl(parser, name, lang_ident)
    }

    pub fn parser_body(&self, language: &Language) -> TokenStream {
        self.0.parser_body(language)
    }
}

impl FromMeta for Node {
    fn from_list(list: &MetaList, name: Option<&Ident>) -> Result<Self, ElementError> {
        let input = list.tokens.to_string();
        Ok(Self::from_string(
            input,
            name.expect("todo name argument is option error handling"),
        )?
        .into())
    }
}

impl LanguageElement for Node {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &self.name());

        let code = self.parser(language);
        Ok(quote! {
            #def
            #code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_NODE
    }

    fn name(&self) -> &Ident {
        self.0.name()
    }
}
