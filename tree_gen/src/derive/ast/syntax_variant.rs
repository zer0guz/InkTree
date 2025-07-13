use std::{collections::HashSet, mem};

use itertools::{Either, Itertools};
use proc_macro2::TokenStream;
use snafu::{ResultExt, Snafu};
use syn::{Ident, Meta, Variant, punctuated::Punctuated, token::Comma};

use crate::{
    Errors,
    derive::{
        Language, LanguageError,
        ast::AttributeOrProperty,
        attributes::{Attribute, AttributeError, LanguageElement},
        parser::MetaError,
        properties::{Property, Root},
    },
    util::{IteratorExt, transpose_errors},
};

#[derive(Debug, Snafu)]
pub enum VariantError {
    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    Scope { source: syn::Error },

    #[snafu(display(
        " attributes have to be provided as a comma seperated list: {}",
        source
    ))]
    Format { source: syn::Error },

    #[snafu(transparent)]
    Meta { source: MetaError },

    #[snafu(display(
        "tree_gen() has to be called with atleast one property: expected at least one of ['static_token','node','token']"
    ))]
    Empty { source: syn::Error },

    #[snafu(display(
        "tree_gen() has to be called with atleast one property: expected at least one of ['static_token','node','token']"
    ))]
    MultipleAttributes { source: syn::Error },
}

pub struct SyntaxVariant {
    pub attribute: Attribute,
    pub ident: Ident,
    pub properties: HashSet<Property>,
}

impl SyntaxVariant {
    // TODO split into smaller chunks :)
    pub fn from_variant(input: Variant) -> Result<Option<Self>, Errors<VariantError>> {
        let meta_elements = input
            .attrs
            .iter()
            .filter(|attr| attr.meta.path().is_ident("tree_gen"))
            .map(|attr| -> Result<_, MetaError> {
                let list = attr.meta.require_list()?;
                let metas = list.parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)?;

                Ok(metas)
            })
            .flat_map(transpose_errors)
            .map(|meta_or_err| {
                let res = meta_or_err.and_then(|meta| {
                    let element = AttributeOrProperty::from_meta(meta)?;
                    Ok(element)
                });
                match input.fields {
                    syn::Fields::Unit => (),
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &input,
                            "only unit variants supported",
                        )
                        .into());
                    }
                };
                res
            })
            .collect_either()
            .map_err(Errors::map_errors)?;

        let (mut attributes, properties): (Vec<_>, Vec<_>) =
            meta_elements.into_iter().partition_map(|a| match a {
                AttributeOrProperty::Attribute(attribute) => Either::Left(attribute),
                AttributeOrProperty::Property(property) => Either::Right(property),
            });

        let Some(attribute) = attributes.pop() else {
            return Ok(None);
        };

        if !attributes.is_empty() {
            return Err(syn::Error::new_spanned(input.ident, "oups"))
                .context(MultipleAttributesSnafu)?;
        };

        Ok(Some(Self {
            ident: input.ident,
            attribute,
            properties: properties.into_iter().collect(),
        }))
    }

    pub fn codegen(&self, language: &Language) -> Result<TokenStream, AttributeError> {
        self.attribute.codegen(&self, language)
    }

    pub fn verify(&self, language: &mut Language) -> Result<(), Errors<LanguageError>> {
        language
            .idents
            .insert(self.ident.to_string(), self.ident.clone());
        if self.properties.contains(&Property::Root(Root)) {
            language
                .set_root(self.ident.clone())
                .map_err(Errors::from)?;
        };
        self.attribute.verify(&self).map_err(Errors::map_errors)?;
        Ok(())
    }
    pub fn build(&mut self, language: &mut Language) -> Result<(), Errors<LanguageError>> {
        self.attribute
            .build(mem::take(&mut self.properties), &self, language)
    }
}
