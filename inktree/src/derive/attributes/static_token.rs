use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Lit, LitStr, MetaList};

use crate::{
    derive::{
        attributes::allowed::ALLOWED_STATIC_TOKEN,
        parser::FromMeta,
        properties::{try_handle_extra, Operator, Property, PropertyKind},
    }, language::{Element, ElementError, Language, LanguageElement}, AstShape
};

pub struct StaticToken {
    pub text: String,
    name: Ident,
    is_extra: bool,
    ignored: bool,
}

impl StaticToken {
    fn from_lit(lit: &Lit, name: Ident) -> Result<Self, ElementError> {
        let text = match lit {
            Lit::Str(str) => str.value(),
            Lit::ByteStr(_) => todo!("support byte str"),
            _ => todo!("lit type error"),
        };
        Ok(Self {
            text,
            name,
            is_extra: false,
            ignored: false,
        })
    }
}

impl FromMeta for StaticToken {
    fn from_list(list: &MetaList, name: Option<&Ident>) -> Result<Self, ElementError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self {
            text: lit.value(),
            name: name.expect("todo name option").clone(),
            is_extra: false,
            ignored: false,
        }
        .into())
    }

    fn from_name_value(
        name_value: &syn::MetaNameValue,
        name: Option<&Ident>,
    ) -> Result<Self, ElementError> {
        match &name_value.value {
            syn::Expr::Lit(expr_lit) => {
                Ok(Self::from_lit(&expr_lit.lit, name.expect("todo name option").clone())?.into())
            }
            _ => Err(syn::Error::new_spanned(
                &name_value.value,
                "todo better text expr type error",
            ))?,
        }
    }
}

impl LanguageElement for StaticToken {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {
        let text = &self.text;
        let ident = &self.name;
        let lang_ident = &language.ident;

        // Build the parser macro call
        let parser_code = if language.extras.is_empty() || self.is_extra {
            quote! {
                inktree::static_token!(#lang_ident::#ident, #text);
            }
        } else {
            quote! {
                inktree::static_token!(#lang_ident::#ident, #text, has_extras);
            }
        };

        Ok(quote! {
            #parser_code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_STATIC_TOKEN
    }

    fn name(&self) -> &Ident {
        &self.name
    }

    fn build(
        &mut self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        properties.iter().for_each(|property| {
            if let Some(kind) = property.try_as_operator() {
                language.operators.push(Operator {
                    kind: kind,
                    ident: self.name.clone(),
                });
            } else {
                self.is_extra = try_handle_extra(&self.name, property, language);
                self.ignored = property.is_ignored()
            }
        });

        Ok(())
    }

    fn ast_shape(&self, _shapes: &mut HashMap<Ident,&Element>,_language: &Language) -> Option<AstShape>{
        if self.ignored {
            return None;
        }
        Some(AstShape::Token)
    }
}
