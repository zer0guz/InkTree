use syn::Ident;

use crate::{language::ElementError, parser::FromMeta};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Root;

impl FromMeta for Root {
    fn from_path(_: &syn::Path, _name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Self)
    }
}
