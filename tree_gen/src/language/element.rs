use enum_dispatch::enum_dispatch;
use itertools::{Either, Itertools};
use proc_macro2::TokenStream;
use snafu::ResultExt;
use strum::IntoDiscriminant;
use syn::Variant;

use crate::{
    MultipleAttributesSnafu, SemanticError, SyntaxVariant,
    attributes::{
        AttributeOrProperty, Node, StaticToken, SyntaxAttribute, SyntaxProperty,
        SyntaxPropertyKind, Token,
    },
    error::Errors,
    util::IteratorExt,
};

#[enum_dispatch]
pub trait LanguageElement {
    fn codegen(&self, stream: &mut TokenStream);
    fn allowed(&self) -> &'static [SyntaxPropertyKind];
    fn build(&mut self, properties: Vec<SyntaxProperty>, source: Variant);
}


pub fn verify_properties<E: LanguageElement>(
    element: &E,
    properties: &Vec<SyntaxProperty>,
) -> Result<(), Errors<SemanticError>> {
    properties
        .iter()
        .map(|prop| {
            element
                .allowed()
                .contains(&prop.discriminant())
                .then_some(prop)
                .ok_or(SemanticError::FromStr)
        })
        .collect_either()?;

    Ok(())
}
