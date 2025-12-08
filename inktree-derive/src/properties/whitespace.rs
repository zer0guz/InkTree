use syn::Ident;

use crate::{language::ElementError, parser::FromMeta};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct _Whitespace;

impl FromMeta for _Whitespace {
    fn from_path(_: &syn::Path, _name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Self)
    }
}
