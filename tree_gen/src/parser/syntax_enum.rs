use snafu::ResultExt;
use syn::{DeriveInput, Ident, parse::Parse};

use crate::{
    ReprSnafu, SyntaxError, error::Errors, parser::variant::SyntaxVariant, util::IteratorExt,
};

pub struct SyntaxEnum {
    pub variants: Vec<SyntaxVariant>,
}

impl SyntaxEnum {
    pub fn from_input(input: DeriveInput) -> Result<Self, Errors<SyntaxError>> {
        let correct_repr = verify_repr(&input);

        let syn::Data::Enum(syntax) = input.data else {
            return Err(SyntaxError::from(syn::Error::new_spanned(&input, "oups")).into());
        };

        correct_repr?;

        let variants = syntax
            .variants
            .into_iter()
            .map(|variant| SyntaxVariant::from_variant(variant))
            .collect_either_flatten()
            .map_err(Errors::convert_errors::<SyntaxError>)?;

        Ok(Self { variants })
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
