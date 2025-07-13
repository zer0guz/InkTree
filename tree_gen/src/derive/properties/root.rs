use crate::derive::parser::{FromMeta, MetaError};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Root;

impl FromMeta for Root {
    fn from_path(path: &syn::Path) -> Result<Self, MetaError> {
        Ok(Self.into())
    }
}
