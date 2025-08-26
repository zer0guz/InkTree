use syn::Ident;

use crate::{derive::parser::FromMeta, language::ElementError};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Whitespace;

impl FromMeta for Whitespace {
    fn from_path(_: &syn::Path,_name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Self)
    }
}
