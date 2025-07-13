mod keyword;
mod operator;
mod padding;
mod root;

use derive_more::From;
use keyword::Keyword;
use strum::{EnumDiscriminants, EnumString, EnumTryAs};
use syn::Meta;

pub use operator::*;
pub use padding::*;
pub use root::*;

use crate::derive::{
    parser::{FromMeta, MetaError},
    properties::operator::{Infix, Postfix, Prefix},
};


#[derive(Debug, PartialEq, Eq, Hash, EnumDiscriminants, From, EnumTryAs)]
#[strum_discriminants(vis(pub), strum(serialize_all = "snake_case"))]
#[strum_discriminants(name(PropertyKind), derive(EnumString))]
pub enum Property {
    Keyword(Keyword),
    OpPrefix(Prefix),
    OpInfix(Infix),
    OpPostfix(Postfix),
    Root(Root),
    Padded(Padded),
    PaddedBy(PaddedBy),
}
impl Property {
    pub fn try_as_operator(self) -> Option<OperatorKind> {
        match self {
            Property::OpPrefix(prefix) => Some(prefix.into()),
            Property::OpInfix(infix) => Some(infix.into()),
            Property::OpPostfix(postfix) => Some(postfix.into()),
            _ => None,
        }
    }
}

impl PropertyKind {
    pub fn from_meta(self, meta: &Meta) -> Result<Property, MetaError> {
        match self {
            PropertyKind::Keyword => Ok(Keyword::from_meta(meta)?.into()),
            PropertyKind::Root => Ok(Root::from_meta(meta)?.into()),
            PropertyKind::OpPrefix => Ok(Prefix::from_meta(meta)?.into()),
            PropertyKind::OpInfix => Ok(Infix::from_meta(meta)?.into()),
            PropertyKind::OpPostfix => Ok(Postfix::from_meta(meta)?.into()),
            PropertyKind::Padded => Ok(Padded::from_meta(meta)?.into()),
            PropertyKind::PaddedBy => Ok(PaddedBy::from_meta(meta)?.into()),
        }
    }
}
