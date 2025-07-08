use std::collections::HashMap;

use proc_macro2::{TokenStream, TokenTree};
use snafu::ResultExt;
use syn::Ident;

use crate::{
    attributes::{properties::SyntaxPropertyKind, AttributeError, SyntaxProperty}, parser::FromMeta, LanguageElement
};

#[derive(Debug)]
pub struct Token {
    pub text: String,
}

impl FromMeta for Token {
    fn from_meta(meta: &syn::Meta) -> Result<super::SyntaxAttribute, AttributeError> {
        match meta {
            syn::Meta::Path(path) => Self::from_path(path),
            syn::Meta::List(meta_list) => Self::from_list(meta_list),
            syn::Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value),
        }
    }

    fn from_list(list: &syn::MetaList) -> Result<super::SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(list, "todo")).context(crate::parser::ListSnafu)?
    }

    fn from_path(path: &syn::Path) -> Result<super::SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(path, "todo")).context(crate::parser::PathSnafu)?
    }

    fn from_name_value(name_value: &syn::MetaNameValue) -> Result<super::SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(name_value, "todo")).context(crate::parser::NameValueSnafu)?
    }
}

impl LanguageElement for Token {
    fn codegen(&self, ident: &Ident,lang_ident:&Ident,idents: &HashMap<String,Ident>) -> Result<TokenStream,AttributeError> {
        todo!()
    }

    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        &[]
    }
}
