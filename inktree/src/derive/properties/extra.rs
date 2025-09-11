use syn::Ident;

use crate::{
    derive::{parser::FromMeta, properties::Property},
    language::{ElementError, Language},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Extra;

impl FromMeta for Extra {
    fn from_path(_: &syn::Path, _name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Self)
    }
}

pub(crate) fn try_handle_extra(name: &Ident, property: &Property, language: &mut Language) -> bool {
    if let Some(_) = property.try_as_extra() {
        language.extras.push(name.clone());
        true
    } else {
        false
    }
}
