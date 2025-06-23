use enum_dispatch::enum_dispatch;
use itertools::{Either, Itertools};
use proc_macro2::TokenStream;
use quote::quote;
use snafu::ResultExt;
use strum::IntoDiscriminant;
use syn::{spanned::Spanned, Variant};

use crate::{
    SemanticError, SyntaxVariant,
    attributes::{
        AttributeOrProperty, Node, StaticToken, SyntaxAttribute, SyntaxProperty,
        SyntaxPropertyKind, Token,
    },
    error::Errors,
    language::{error::MultipleAttributesSnafu},
    util::IteratorExt,
};


#[enum_dispatch]
pub trait LanguageElement {
    fn codegen(&self, stream: &mut TokenStream);
}

//     fn verify_properties(
//         attribute: &SyntaxAttribute,
//         properties: &Vec<SyntaxProperty>,
//     ) -> Result<(), Errors<SemanticError>> {
//         properties
//             .into_iter()
//             .map(|prop| {
//                 attribute
//                     .allowed()
//                     .contains(&prop.discriminant())
//                     .then_some(prop)
//                     .ok_or(SemanticError::FromStr)
//             })
//             .collect_either()?;

//         Ok(())
//     }

pub trait Verify: Sized {
    const ALLOWED: &[SyntaxPropertyKind];
}

impl SyntaxAttribute {
    const fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        match self {
            SyntaxAttribute::StaticToken(_) => StaticToken::ALLOWED,
            SyntaxAttribute::Node(_) => Node::ALLOWED,
            SyntaxAttribute::Token(_) => Token::ALLOWED,
        }
    }
}
