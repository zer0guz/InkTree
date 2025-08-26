use syn::Ident;

use crate::{derive::parser::FromMeta, language::ElementError};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Keyword {}

impl FromMeta for Keyword {
    fn from_path(_: &syn::Path,_name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Keyword {})
    }
}
