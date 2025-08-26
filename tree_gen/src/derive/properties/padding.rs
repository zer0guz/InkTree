use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, parse::Parse};

use crate::{derive::parser::FromMeta, language::ElementError};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Padded {}

pub fn _padded(inner: TokenStream) -> TokenStream {
    quote! {#inner.padded_by()}
}

impl FromMeta for Padded {
    fn from_path(_: &syn::Path,_name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Padded {})
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PaddedBy {
    pub padding: Ident,
}

impl FromMeta for PaddedBy {
    fn from_list(list: &syn::MetaList,_name: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(Self {
            padding: list.parse_args_with(Ident::parse)?,
        })
    }
}
