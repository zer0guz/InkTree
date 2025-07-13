mod node;
mod pratt;
mod static_token;
mod token;

use std::collections::HashSet;

use crate::{
    Errors,
    derive::{
        Language, LanguageError,
        ast::SyntaxVariant,
        parser::{FromMeta, MetaError},
        properties::{Property, PropertyKind},
    },
    util::IteratorExt,
};
use enum_dispatch::enum_dispatch;
use proc_macro2::TokenStream;
use snafu::{ResultExt, Snafu};
use strum::{EnumDiscriminants, EnumString, IntoDiscriminant};
use syn::Meta;

pub use node::*;
pub use pratt::*;
pub use static_token::*;
pub use token::*;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(self)))]
pub enum AttributeError {
    #[snafu(display("'{}': unknown property! expected one of: {}",source.to_string(), "todo!"))]
    Unsupported { source: syn::Error },
}

#[derive(EnumDiscriminants)]
#[strum_discriminants(vis(pub), strum(serialize_all = "snake_case"))]
#[strum_discriminants(name(AttributeKind), derive(EnumString))]
#[enum_dispatch(LanguageElement)]
pub enum Attribute {
    StaticToken(StaticToken),
    Node(Node),
    Token(Token),
    Pratt(Pratt),
    Inline(Inline),
}

impl AttributeKind {
    pub fn from_meta(self, meta: &Meta) -> Result<Attribute, MetaError> {
        match self {
            AttributeKind::StaticToken => Ok(StaticToken::from_meta(meta)?.into()),
            AttributeKind::Node => Ok(Node::from_meta(meta)?.into()),
            AttributeKind::Token => Ok(Token::from_meta(meta)?.into()),
            AttributeKind::Pratt => Ok(Pratt::from_meta(meta)?.into()),
            AttributeKind::Inline => Ok(Inline::from_meta(meta)?.into()),
        }
    }
}

#[enum_dispatch]
pub trait LanguageElement: Sized {
    fn codegen(
        &self,
        variant: &SyntaxVariant,
        language: &Language,
    ) -> Result<TokenStream, AttributeError>;

    fn build(
        &self,
        _: HashSet<Property>,
        _: &SyntaxVariant,
        _: &mut Language,
    ) -> Result<(), Errors<LanguageError>> {
        Ok(())
    }

    fn verify(&self, variant: &SyntaxVariant) -> Result<(), Errors<AttributeError>> {
        variant
            .properties
            .iter()
            .map(|prop| {
                self.allowed()
                    .contains(&prop.discriminant())
                    .then_some(prop)
                    .ok_or(syn::Error::new_spanned(&variant.ident, "todo text verify"))
                    .context(UnsupportedSnafu)
            })
            .collect_either()?;

        Ok(())
    }
    fn allowed(&self) -> &'static [PropertyKind];
}
