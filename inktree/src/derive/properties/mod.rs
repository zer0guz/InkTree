mod delimiters;
mod extra;
mod keyword;
mod operator;
mod padding;
mod root;
mod whitespace;

use std::str::FromStr;

use derive_more::From;
use strum::{EnumDiscriminants, EnumString, EnumTryAs};
use syn::Meta;

pub use extra::*;
pub use operator::*;
pub use root::*;

use crate::{
    derive::{
        parser::FromMeta,
        properties::{
            extra::Extra,
            operator::{Infix, Postfix, Prefix},
        },
    },
    language::ElementError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumDiscriminants, From, EnumTryAs)]
#[strum_discriminants(vis(pub), strum(serialize_all = "snake_case"))]
#[strum_discriminants(name(PropertyKind), derive(EnumString))]
pub enum Property {
    // Keyword(Keyword),
    OpPrefix(Prefix),
    OpInfix(Infix),
    OpPostfix(Postfix),
    Root(Root),
    // Padded(Padded),
    // PaddedBy(PaddedBy),
    // DelimitedBy(DelimitedBy),
    // Whitespace(Whitespace),
    Extra(Extra),
}
impl Property {
    pub fn try_as_operator(&self) -> Option<OperatorKind> {
        match self {
            Property::OpPrefix(prefix) => Some((*prefix).into()),
            Property::OpInfix(infix) => Some((*infix).into()),
            Property::OpPostfix(postfix) => Some((*postfix).into()),
            _ => None,
        }
    }

    pub fn from_meta(meta: Meta) -> Result<Property, ElementError> {
        let kind = meta.path().get_ident().expect("asdsad");
        if let Ok(kind) = PropertyKind::from_str(&kind.to_string().as_str()) {
            Ok(kind.from_meta(&meta)?)
        } else {
            Err(syn::Error::new_spanned(kind, "unsupported todo text").into())
        }
    }
}

impl PropertyKind {
    pub fn from_meta(self, meta: &Meta) -> Result<Property, ElementError> {
        use PropertyKind as Pk;
        match self {
            // Pk::Keyword => Ok(Keyword::from_meta(meta, None)?.into()),
            Pk::Root => Ok(Root::from_meta(meta, None)?.into()),
            Pk::OpPrefix => Ok(Prefix::from_meta(meta, None)?.into()),
            Pk::OpInfix => Ok(Infix::from_meta(meta, None)?.into()),
            Pk::OpPostfix => Ok(Postfix::from_meta(meta, None)?.into()),
            // Pk::Padded => Ok(Padded::from_meta(meta, None)?.into()),
            // Pk::PaddedBy => Ok(PaddedBy::from_meta(meta, None)?.into()),
            // Pk::DelimitedBy => Ok(DelimitedBy::from_meta(meta, None)?.into()),
            // Pk::Whitespace => Ok(Whitespace::from_meta(meta, None)?.into()),
            Pk::Extra => Ok(Extra::from_meta(meta, None)?.into()),
        }
    }
}
