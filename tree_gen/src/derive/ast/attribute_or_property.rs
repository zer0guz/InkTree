use std::str::FromStr;

use snafu::{OptionExt, ResultExt, Snafu};
use syn::Meta;

use crate::derive::{
    attributes::{Attribute, AttributeError, AttributeKind},
    parser::MetaError,
    properties::{Property, PropertyError, PropertyKind},
};


pub enum AttributeOrProperty {
    Attribute(Attribute),
    Property(Property),
}

impl AttributeOrProperty {
    pub fn from_meta(meta: Meta) -> Result<Self, MetaError> {
        let Some(ident) = meta.path().get_ident() else {
            return Err(
                syn::Error::new_spanned(meta.path(), "Path is not an identifier TODO text").into(),
            )
        };

        if let Ok(kind) = AttributeKind::from_str(&ident.to_string().as_str()) {
            Ok(Self::Attribute(kind.from_meta(&meta)?))
        } else if let Ok(kind) = PropertyKind::from_str(&ident.to_string().as_str()) {
            Ok(Self::Property(kind.from_meta(&meta)?))
        } else {
            Err(syn::Error::new_spanned(ident, "unsupported todo text").into())
        }
    }
}
