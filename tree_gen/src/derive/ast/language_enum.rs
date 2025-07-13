use snafu::{ResultExt, Snafu};
use syn::{DeriveInput, Ident, parse::Parse};

use crate::{
    derive::ast::{SyntaxVariant, VariantError},
    error::Errors,
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
pub enum EnumError {
    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    Data { source: syn::Error },

    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    Repr { source: syn::Error },

    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    #[snafu(context(false))]
    Variant { source: VariantError },
}

pub struct LanguageEnum {
    pub variants: Vec<SyntaxVariant>,
    pub ident: Ident,
}

impl LanguageEnum {
    pub fn from_input(input: DeriveInput) -> Result<Self, Errors<EnumError>> {
        let correct_repr = verify_repr(&input);

        let syn::Data::Enum(syntax) = input.data else {
            Err(syn::Error::new_spanned(&input, "oups")).context(DataSnafu)?
        };

        correct_repr?;

        let variants = syntax
            .variants
            .into_iter()
            .map(|variant| SyntaxVariant::from_variant(variant))
            .collect_either_flatten_into()?
            .into_iter()
            .flatten()
            .collect();

        Ok(Self {
            variants,
            ident: input.ident,
        })
    }
}

fn verify_repr(input: &DeriveInput) -> Result<(), EnumError> {
    let repr_inner = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("repr"))
        .next()
        .ok_or_else(|| syn::Error::new_spanned(input, "todo text repr"))
        .context(ReprSnafu)?
        .parse_args_with(Ident::parse)
        .context(ReprSnafu)?;

    if !(repr_inner == "u32") {
        return Err(syn::Error::new_spanned(input, "todo text repr2")).context(ReprSnafu);
    };

    Ok(())
}
