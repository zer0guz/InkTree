use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, parse::Parse};

use crate::{language::ElementError, parser::FromMeta};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct _Padded {}

pub fn _padded(inner: TokenStream) -> TokenStream {
    quote! {#inner.padded_by()}
}

impl FromMeta for _Padded {
    fn from_path(_: &syn::Path, _name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(_Padded {})
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct _PaddedBy {
    pub padding: Ident,
}

impl FromMeta for _PaddedBy {
    fn from_list(list: &syn::MetaList, _name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Self {
            padding: list.parse_args_with(Ident::parse)?,
        })
    }
}
