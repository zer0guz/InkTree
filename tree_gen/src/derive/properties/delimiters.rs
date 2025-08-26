use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, punctuated::Punctuated, token::Comma};

use crate::{derive::parser::FromMeta, language::ElementError};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct DelimitedBy {
    pub open: Ident,
    pub close: Ident,
}

impl DelimitedBy {
    pub fn parser(&self, inner: &TokenStream) -> TokenStream {
        let Self { open, close } = self;

        quote! {#inner.padded_by(#open::parser(),#close::parser())}
    }
}

impl FromMeta for DelimitedBy {
    fn from_list(list: &syn::MetaList,_name: Option<&Ident>) -> Result<Self, ElementError> {
        let (open, close) = list
            .parse_args_with(Punctuated::<Ident, Comma>::parse_terminated)?
            .into_iter()
            .collect_tuple()
            .expect("todo error handling argument count");
        Ok(Self { open, close })
    }
}
