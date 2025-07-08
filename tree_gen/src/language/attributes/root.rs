use std::collections::HashMap;

use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use snafu::ResultExt;
use syn::{Ident, MetaList};

use crate::{
    attributes::{properties::SyntaxPropertyKind, SyntaxProperty}, language::{attributes::{AttributeError, SyntaxAttribute}, code::struct_def}, parser::FromMeta, LanguageElement
};

#[derive(Debug)]
pub struct Root;

impl FromMeta for Root {
    fn from_path(path: &syn::Path) -> Result<SyntaxAttribute, AttributeError> {
        Ok(Self.into())
    }
}

impl LanguageElement for Root {
    fn codegen(&self, ident: &Ident,lang_ident:&Ident,idents: &HashMap<String,Ident>) -> Result<TokenStream,AttributeError> {
        let body = quote! {};
        Ok(struct_def(body, &ident))
    }

    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        &[]
    }
}
