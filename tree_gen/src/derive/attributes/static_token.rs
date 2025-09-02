use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Lit, LitStr, MetaList};

use crate::{
    derive::{
        attributes::allowed::ALLOWED_STATIC_TOKEN,
        language::{parseable_impl, struct_def},
        parser::FromMeta,
        properties::{Operator, Property, PropertyKind},
    },
    language::{ElementError, Language, LanguageElement},
};

pub struct StaticToken {
    pub text: String,
    name: Ident,
}

impl StaticToken {
    fn from_lit(lit: &Lit, name: Ident) -> Result<Self, ElementError> {
        let text = match lit {
            Lit::Str(str) => str.value(),
            Lit::ByteStr(_) => todo!("support byte str"),
            _ => todo!("lit type error"),
        };
        Ok(Self { text, name })
    }
}

impl FromMeta for StaticToken {
    fn from_list(list: &MetaList, name: Option<&Ident>) -> Result<Self, ElementError> {
        let lit: LitStr = list.parse_args()?;
        Ok(Self {
            text: lit.value(),
            name: name.expect("todo name option").clone(),
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
        let def_body = quote! {};
        let def = struct_def(def_body, &self.name);
        let impl_code = static_token_impl(&self.text, &self.name, &language.ident);
        Ok(quote! {
            #def
            #impl_code
        })
    }

    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_STATIC_TOKEN
    }

    fn name(&self) -> &Ident {
        &self.name
    }

    fn build(
        &self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        properties.iter().for_each(|prop| {
            if let Some(kind) = prop.try_as_operator() {
                language.operators.push(Operator {
                    kind,
                    ident: self.name.clone(),
                });
            };
        });

        Ok(())
    }
}

pub fn static_token_impl(text: &str, ident: &Ident, lang_ident: &Ident) -> TokenStream {
    let parser = quote! {
        use ::tree_gen::chumksy_ext::BuilderParser;
        ::tree_gen::chumsky::prelude::just(#text).as_static_token(#lang_ident::#ident)
    };
    parseable_impl(parser, ident, lang_ident)
}
