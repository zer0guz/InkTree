use enum_dispatch::enum_dispatch;
use itertools::{Either, Itertools};
use proc_macro2::TokenStream;
use snafu::ResultExt;
use strum::IntoDiscriminant;
use syn::{Ident};

use crate::{
    MultipleAttributesSnafu, SemanticError, SyntaxVariant,
    attributes::{
        AttributeOrProperty, Node, StaticToken, SyntaxAttribute, SyntaxProperty,
        SyntaxPropertyKind, Token,
    },
    error::Errors,
    util::IteratorExt,
};

pub struct Element {
    attribute: SyntaxAttribute,
    properties: Vec<SyntaxProperty>,
    ident: Ident,
}

impl Element {
    pub fn from_variant(variant: SyntaxVariant) -> Result<Self, Errors<SemanticError>> {
        use AttributeOrProperty::*;
        let (mut attributes, properties): (Vec<_>, Vec<_>) = variant
            .meta_elements
            .into_iter()
            .partition_map(|a| match a {
                Attribute(attribute) => Either::Left(attribute),
                Property(property) => Either::Right(property),
            });

        let Some(attribute) = attributes.pop() else {
            todo!("empty variant")
        };

        if !attributes.is_empty() {
            return Err(syn::Error::new_spanned(variant.source, "oups"))
                .context(MultipleAttributesSnafu)?;
        };

        verify_properties(&attribute, &properties)?;

        Ok(Element {
            attribute: attribute,
            properties: properties,
            ident: variant.source.ident,
        })
    }
    pub fn codegen(&self, stream: &mut TokenStream) {
        self.attribute.codegen(&self.properties,&self.ident,stream)
    }
}

#[enum_dispatch]
pub trait LanguageElement {
    fn codegen(&self, properties: &Vec<SyntaxProperty>, ident: &Ident, stream: &mut TokenStream);
    fn allowed(&self) -> &'static [SyntaxPropertyKind];
}

pub fn verify_properties(
    element: &SyntaxAttribute,
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
