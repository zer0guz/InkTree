use crate::derive::parser::FromMeta;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Keyword {}

impl FromMeta for Keyword {
    fn from_path(path: &syn::Path) -> Result<Self, crate::derive::parser::MetaError> {
        Ok(Keyword {})
    }
}
