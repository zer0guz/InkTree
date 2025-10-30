use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Ident, MetaList};

use crate::{
    derive::{
        attributes::{allowed::ALLOWED_PRATT, Node},
        parser::FromMeta,
        properties::{Property, PropertyKind},
    }, language::{Element, ElementError, Language, LanguageElement}, AstGenCtx, AstShape, AstShapeKind
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

        let pratt = quote! {
            pratt_ext(#base_body,#lang_ident::#name_ident)
        };

        self.node.0.parser(pratt, language, true)
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
        &mut self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        self.node.build(properties, language)
    }

    fn ast_shape(
        &self,
        ast_shapes: &mut HashMap<Ident, AstShape>,
        language: &Language,
    ) -> Option<AstShape> {
        let mut ctx = AstGenCtx::new();
        let atom_name = format_ident!("{}Atom",self.name());
        let atom_shape = self.node.0.dsl.ast_shape(&atom_name, &mut ctx, ast_shapes);
        ast_shapes.insert(atom_name.clone(), atom_shape);

        Some(AstShape::new(AstShapeKind::Pratt {
            atom: atom_name,
            prefix_ops: language
                .operators
                .iter()
                .filter(|op| op.is_prefix())
                .map(|op| op.ident.clone())
                .collect(),
            infix_ops: language
                .operators
                .iter()
                .filter(|op| op.is_infix())
                .map(|op| op.ident.clone())
                .collect(),
        },self.name().clone()))
    }
}
