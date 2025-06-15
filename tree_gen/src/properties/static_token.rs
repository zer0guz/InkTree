
use syn::LitStr;

use crate::{properties::{FromMeta, PropertyError}, SyntaxAttribute};


#[derive(Debug)]
pub struct StaticToken {
    pub ident: syn::Ident,
    pub parser: (),
}

impl StaticToken {
    pub fn new(ident: syn::Ident, parser: ()) -> Self {
        Self { ident, parser }
    }
}

impl<'a> From<StaticToken> for SyntaxAttribute {
    fn from(value: StaticToken) -> Self {
        Self::StaticToken(value)
    }
}

impl FromMeta for StaticToken {
    fn from_meta(meta: syn::Meta) -> Result<Self, PropertyError> {
        match meta {
            syn::Meta::Path(path) => Self::from_path(path),
            syn::Meta::List(meta_list) => Self::from_list(meta_list),
            syn::Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value),
        }
    }

    fn from_list(list: syn::MetaList) -> Result<Self, PropertyError> {
        let text: LitStr = list.parse_args()?;

        todo!()
    }
    
    fn from_name_value(name_value: syn::MetaNameValue) -> Result<Self, PropertyError> {
        todo!("ho")
    }

}
