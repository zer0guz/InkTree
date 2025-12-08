use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, MetaList};

use crate::{
    ast::Shape,
    attributes::{Node, SyntaxAttribute, allowed::ALLOWED_PRATT},
    language::{Element, ElementError, Language, LanguageElement},
    parser::{DslExpr, FromMeta},
    properties::{Property, PropertyKind},
};

#[derive(Debug)]
pub(crate) struct Pratt {
    pub node: Node,
}

impl Pratt {
    pub fn code(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;
        let name_ident = &self.name();

        // base body from Rule
        let base_body = self.node.0.parser_body(language);

        let pratt = quote! {
            {pratt_ext(#base_body,#lang_ident::#name_ident)}
        };

        self.node.0.parser(pratt, language, true)
    }
}

impl FromMeta for Pratt {
    fn from_list(list: &MetaList, name: Option<&Ident>) -> Result<Self, ElementError> {
        let input = list.tokens.to_string();
        let node = Node::from_string(input, name.expect("asdasd"))?;
        let DslExpr::Just(_) = &node.0.dsl else {
            return Err(syn::Error::new_spanned(
                &list,
                "pratt atom has to be exactly one other syntax element",
            ))
            .map_err(ElementError::from)?;
        };
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
        if let DslExpr::Just(ident) = &self.node.0.dsl
            && let Element {
                attribute: _rule_attr @ SyntaxAttribute::Rule(_),
                ..
            } = language.element_by_name(ident)
        {
            return Err(syn::Error::new_spanned(
                &ident,
                "pratt atom cannot be a rule! it has to be its own CST node",
            ))
            .map_err(ElementError::from)?;
        }
        self.node.build(properties, language)
    }

    fn ast_shape(&self, language: &Language) -> Option<Shape> {
        //LowerCtx::new(language).lower_pratt(self)

        todo!("ast shape pratt")
    }
}
