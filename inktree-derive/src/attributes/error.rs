use proc_macro2::{Span, TokenStream};
use quote::quote;
use snafu::ResultExt;
use syn::Ident;

use crate::{
    ast::Shape,
    language::{ElementError, Language, LanguageElement},
    parser::FromMeta,
    properties::PropertyKind,
};

#[derive(Debug, Clone)]
pub struct Error {
    name: Ident,
}

impl FromMeta for Error {
    fn from_path(path: &syn::Path, ident: Option<&Ident>) -> Result<Self, ElementError> {
        if path.is_ident(&Ident::new("error", Span::call_site())) {
            return Ok(Self {
                name: ident.unwrap().clone(),
            });
        } else {
            Err(syn::Error::new_spanned(path, "error parsing meta"))
                .context(crate::language::MetaSnafu)?
        }
    }
}

impl LanguageElement for Error {
    fn codegen(&self, _language: &Language) -> Result<TokenStream, ElementError> {
        let name = &self.name;
        Ok(quote! {
            #[derive(Debug)]
            pub struct #name;
        })
    }

    fn name(&self) -> &Ident {
        &self.name
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        &[]
    }

    fn ast_shape(&self, _language: &Language) -> Option<Shape> {
        None
    }
}
