use std::collections::HashMap;

use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use snafu::ResultExt;
use syn::{Ident, Lit, LitStr, MetaList};

use crate::derive::{attributes::{AttributeError, LanguageElement}, codegen::{parseable_impl, struct_def}, parser::{FromMeta, MetaError}, properties::PropertyKind};


const ALLOWED: &[PropertyKind] = &[];

pub struct StaticToken {
    pub text: String,
}

impl StaticToken {
    fn from_lit(lit: &Lit) -> Result<Self, MetaError> {
        let text = match lit {
            Lit::Str(str) => str.value(),
            Lit::ByteStr(_) => todo!("support byte str"),
            _ => todo!("lit type error"),
        };
        Ok(Self { text })
    }
}

impl FromMeta for StaticToken {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self { text: lit.value() }.into())
    }


    fn from_name_value(name_value: &syn::MetaNameValue) -> Result<Self, MetaError> {
        match &name_value.value {
            syn::Expr::Lit(expr_lit) => Ok(Self::from_lit(&expr_lit.lit)?.into()),
            _ => Err(syn::Error::new_spanned(
                &name_value.value,
                "todo better text expr type error",
            ))?,
        }
    }
}

impl LanguageElement for StaticToken {
    fn codegen(
        &self,
        ident: &Ident,
        lang_ident: &Ident,
        _: &HashMap<String, Ident>,
    ) -> Result<TokenStream, AttributeError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &ident);
        let impl_code = static_token_impl(&self.text, ident, lang_ident);
        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        &[]
    }
}

pub fn static_token_impl(text: &str, ident: &Ident, lang_ident: &Ident) -> TokenStream {
    let parser = quote! {
        use ::tree_gen::BuilderParser;
        ::tree_gen::chumsky::prelude::just(#text).as_static_token(#lang_ident::#ident)
    };
    parseable_impl(parser, ident, lang_ident)
}

