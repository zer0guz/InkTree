use derive_more::From;
use snafu::Snafu;
use strum::{EnumDiscriminants, EnumString};
use syn::Meta;

use crate::derive::{attributes::{Root, StaticToken}, parser::{FromMeta, MetaError}};

#[derive(Debug, Snafu)]
pub enum PropertyError {
    #[snafu(transparent)]
    Meta { source: MetaError },
}

#[derive(Debug, PartialEq, Eq, EnumDiscriminants, From)]
#[strum(serialize_all = "kebab-case")]
#[strum_discriminants(vis(pub))]
#[strum_discriminants(name(PropertyKind), derive(EnumString))]
pub enum Property {
    None(()),
}

impl PropertyKind {
    pub fn from_meta(self, meta: &Meta) -> Result<Property, MetaError> {
        match self {
            _ => todo!("property from meta dispatch")
        }
    }
}
