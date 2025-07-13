use crate::derive::parser::FromMeta;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Padded {}

impl FromMeta for Padded {
    fn from_path(_: &syn::Path) -> Result<Self, crate::derive::parser::MetaError> {
        Ok(Padded {})
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PaddedBy {}

impl FromMeta for PaddedBy {
    fn from_path(_: &syn::Path) -> Result<Self, crate::derive::parser::MetaError> {
        Ok(PaddedBy {})
    }
}
