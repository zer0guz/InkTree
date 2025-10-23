use std::collections::HashSet;

use chumsky::Parser;
use derive_more::From;
use proc_macro2::TokenStream;
use quote::{quote};
use syn::{Ident, MetaList};

use crate::{
    derive::{
        attributes::{allowed::ALLOWED_NODE, rule::Rule},
        parser::{dsl_lexer, dsl_parser, FromMeta},
        properties::{Property, PropertyKind},
    },
    language::{ElementError, Language, LanguageElement}, AstGenCtx, AstShape,
};

#[derive(Debug, From)]
pub struct Node(pub Rule);

impl Node {
    pub fn from_string(input: String, name: &Ident) -> Result<Self, syn::Error> {
        let tokens = dsl_lexer()
            .parse(&input)
            .into_result()
            .expect("lexer error todo");
        let dsl = dsl_parser()
            .parse(&tokens)
            .into_result()
            .expect("parser error todo")
            .into();
        Ok(Self(Rule::new(dsl, name.clone(), HashSet::new())))
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
        let name = &self.name();
        let lang_ident = &language.ident;

        let code = self.0.parser_body(language);

        // let ast_defs: Vec<TokenStream> = shapes
        //     .into_iter()
        //     .map(|shape| shape.codegen(lang_ident,name))
        //     .collect();

        Ok(quote! {
            ::inktree::node!(#lang_ident::#name,{#code});
            //#(#ast_defs)*

        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_NODE
    }

    fn name(&self) -> &Ident {
        self.0.name()
    }

    fn build(
        &mut self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        self.0.build(properties, language)
    }
    
    fn ast_shape(&self,language: &mut Language) -> Option<AstShape> {
        
        let mut ctx = AstGenCtx::new();
        Some(self.0.dsl.ast_shape(self.name(), &mut ctx,&mut language.ast_shapes))

    }
}
