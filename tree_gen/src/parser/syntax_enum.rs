use snafu::ResultExt;
use syn::{Attribute, DeriveInput, Ident, parse::Parse};

use crate::{
    ReprSnafu, SyntaxError, error::Errors, parser::variant::SyntaxVariant, util::IteratorExt,
};

pub struct SyntaxEnum<'a> {
    pub ident: &'a Ident,
    pub variants: Vec<SyntaxVariant<'a>>,
    pub source: &'a DeriveInput,
}

impl<'a> SyntaxEnum<'a> {
    pub fn from_input(input: &'a DeriveInput) -> Result<Self, Errors<SyntaxError>> {
        let syn::Data::Enum(syntax) = &input.data else {
            return Err(SyntaxError::from(syn::Error::new_spanned(input, "oups")))?;
        };

        let variants = syntax
            .variants
            .iter()
            .map(|variant| SyntaxVariant::from_variant(variant))
            .collect_either_flatten()
            .map_err(Errors::convert_errors::<SyntaxError>)?;

        verify_repr(input)?;

        Ok(Self {
            ident: &input.ident,
            variants,
            source: input,
        })
    }
}

fn verify_repr(input: &DeriveInput) -> Result<(), SyntaxError> {
    let repr_inner = input
        .attrs
        .iter()
        .filter(|&attr| attr.path().is_ident("repr"))
        .next()
        .ok_or_else(|| syn::Error::new_spanned(input, "todo text"))
        .context(ReprSnafu { message: "no repr" })?
        .parse_args_with(Ident::parse)
        .context(ReprSnafu {
            message: "invalid repr",
        })?;

    if !(repr_inner == "u32") {
        return Err(SyntaxError::from(syn::Error::new_spanned(input, "oups")))?;
    };

    Ok(())
}
