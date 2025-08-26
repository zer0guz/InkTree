use enum_dispatch::enum_dispatch;
use proc_macro2::TokenStream;
use quote::ToTokens;
use snafu::{ResultExt, Snafu};
use strum::IntoDiscriminant;
use syn::{Attribute, Ident, Meta, MetaList, Variant, punctuated::Punctuated, token::Comma};

use crate::{
    Errors,
    derive::{
        attributes::SyntaxAttribute,
        properties::{Property, PropertyKind, Root},
    },
    language::Language,
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum ElementError {
    #[snafu(display(
        " attributes have to be provided as a comma seperated list: {}",
        source
    ))]
    Format { source: syn::Error },
    #[snafu(context(false))]
    Custom { source: syn::Error },

    #[snafu(display(" some error parsing the meta (todo) {}", source))]
    Meta { source: syn::Error },

    #[snafu(display("'{}': unknown property! expected one of: {}",source.to_string(), "todo!"))]
    Unsupported { source: syn::Error },
}

pub struct SyntaxElement {
    pub attribute: SyntaxAttribute,
    pub properties: Vec<Property>,
}

impl SyntaxElement {
    pub fn from_variant(variant: &Variant) -> Result<Option<Self>, Errors<ElementError>> {
        match variant.fields {
            syn::Fields::Unit => (),
            _ => {
                return Err(syn::Error::new_spanned(
                    &variant,
                    "only unit variants supported",
                ))
                .map_err(ElementError::from)?;
            }
        };

        let mut attributes = variant
            .attrs
            .iter()
            .filter(|attr| attr.meta.path().is_ident("tree_gen"));

        let Some(attribute) = attributes.next() else {
            return Ok(None);
        };

        let list = attribute.meta.require_list().context(FormatSnafu)?;

        let mut meta_iter = list
            .parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)
            .context(FormatSnafu)?
            .into_iter();

        let attribute_meta = meta_iter
            .next()
            .ok_or(syn::Error::new_spanned(
                &list,
                "need exactly one attribute (todo)",
            ))
            .context(MetaSnafu)?;

        let properties = meta_iter
            .map(|meta| Property::from_meta(meta))
            .collect_either()
            .map_err(Errors::map_errors)?;

        let syntax_attribute = SyntaxAttribute::from_meta(&attribute_meta, Some(&variant.ident))?; //SyntaxAttribute::from_meta(meta, name)?;

        Ok(Some(Self {
            attribute: syntax_attribute,
            properties,
        }))
    }

    pub fn from_attribute(attribute: &Attribute) -> Result<Self, ElementError> {
        attribute.into_token_stream();

        let inner: MetaList = attribute
            .meta
            .require_list()
            .context(FormatSnafu)?
            .parse_args()
            .context(FormatSnafu)?;
        // let dsl = list.tokens.to_string();
        let attribute = SyntaxAttribute::from_meta(&inner.into(), None)?;

        Ok(SyntaxElement {
            attribute,
            properties: vec![],
        })
    }

    pub fn build(&self, language: &mut Language) -> Result<(), Errors<ElementError>> {
        self.properties
            .iter()
            .map(|prop| {
                self.attribute
                    .allowed()
                    .contains(&prop.discriminant())
                    .then_some(prop)
                    .ok_or(syn::Error::new_spanned(
                        &self.attribute.name(),
                        "todo text verify",
                    ))
                    .context(UnsupportedSnafu)
            })
            .collect_either()?;

        self.attribute.build(&self.properties, language)?;

        if self.properties.contains(&Property::Root(Root)) {
            language.root_idents.push(self.attribute.name().clone());
        };

        return Ok(());
    }

    pub fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {
        Ok(self.attribute.codegen(language)?)
    }
}

#[enum_dispatch]
pub trait LanguageElement: Sized {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError>;
    fn build(
        &self,
        _properties: &Vec<Property>,
        _language: &mut Language,
    ) -> Result<(), ElementError> {
        Ok(())
    }

    fn name(&self) -> &Ident;

    fn allowed(&self) -> &'static [PropertyKind];
}
