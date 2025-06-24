use snafu::Snafu;
use syn::{Meta, Variant, punctuated::Punctuated, token::Comma};

use crate::{
    attributes::{AttributeError, AttributeOrProperty},
    error::Errors,
    util::{IteratorExt, transpose_errors},
};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
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
    #[snafu(context(false))]
    Format { source: syn::Error },

    #[snafu(display("Error while parsing the property {}", source))]
    #[snafu(context(false))]
    Property { source: AttributeError },

    #[snafu(display(
        "tree_gen() has to be called with atleast one property: expected at least one of ['static_token','node','token']"
    ))]
    Empty { source: syn::Error },
}

pub struct SyntaxVariant {
    pub meta_elements: Vec<AttributeOrProperty>,
    pub source: Variant,
}

impl SyntaxVariant {
    // TODO split into smaller chunks :)
    pub fn from_variant(input: Variant) -> Result<Self, Errors<VariantError>> {
        let meta_elements = input
            .attrs
            .iter()
            .filter(|attr| attr.meta.path().is_ident("tree_gen"))
            .map(|attr| -> Result<_, AttributeError> {
                let list = attr.meta.require_list()?;
                let metas = list.parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)?;

                Ok(metas)
            })
            .flat_map(transpose_errors)
            .map(|meta_or_err| {
                let res = meta_or_err.and_then(|meta| {
                    let prop = AttributeOrProperty::from_meta(meta)?;
                    Ok(prop)
                });
                match input.fields {
                    syn::Fields::Unit => (),
                    _ => {
                        return Err(AttributeError::from(syn::Error::new_spanned(
                            &input,
                            "only unit variants supported",
                        )))?;
                    }
                };
                Ok(res?)
            })
            .collect_either()?;

        Ok(Self {
            meta_elements,
            source: input,
        })
    }
}
