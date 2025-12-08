use chumsky::Parser;
use derive_more::From;
use proc_macro2::TokenStream;
use syn::{Ident, MetaList};

use crate::{
    ast::Shape,
    attributes::{allowed::ALLOWED_NODE, rule::Rule},
    language::{ElementError, Language, LanguageElement},
    parser::{FromMeta, dsl_lexer, dsl_parser},
    properties::{Property, PropertyKind},
};

#[derive(Debug, From)]
pub(crate) struct Node(pub Rule);

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
        Ok(Self(Rule::new(dsl, name.clone(), Vec::new())))
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
        Ok(self.0.parser(self.0.parser_body(language), language, true))
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

    fn ast_shape(&self, language: &Language) -> Option<Shape> {
        // // If the DSL fully prunes (e.g., only ignored tokens), drop the node.
        // let mut lc = LowerCtx::new(language);
        // lc.lower_rule_dsl(self.name(), &self.0.dsl)

        todo!("ast shape node")
    }
}
