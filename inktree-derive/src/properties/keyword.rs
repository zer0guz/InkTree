use syn::Ident;

use crate::{language::ElementError, parser::FromMeta};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct _Keyword {}

impl FromMeta for _Keyword {
    fn from_path(_: &syn::Path, _name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(_Keyword {})
    }
}
