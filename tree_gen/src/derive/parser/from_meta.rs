use snafu::{ResultExt, Snafu};
use syn::{Meta, MetaList, MetaNameValue, Path};

use crate::derive::attributes::{Attribute, AttributeError, NodeError, TokenError};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum MetaError {
    #[snafu(transparent)]
    Meta { source: syn::Error },
    #[snafu(transparent)]
    Node { source: NodeError },
    #[snafu(transparent)]
    Token { source: TokenError },

    #[snafu(display("list not supported for this property todo"))]
    Attribute { source: AttributeError },
}

pub trait FromMeta: Sized {
    fn from_meta(meta: &Meta) -> Result<Self, MetaError> {
        match meta {
            Meta::Path(path) => Self::from_path(path),
            Meta::List(meta_list) => Self::from_list(meta_list),
            Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value),
        }
    }

    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        Err(syn::Error::new_spanned(list, "todo"))?
    }

    fn from_path(path: &Path) -> Result<Self, MetaError> {
        Err(syn::Error::new_spanned(path, "todo"))?
    }
    fn from_name_value(name_value: &MetaNameValue) -> Result<Self, MetaError> {
        Err(syn::Error::new_spanned(name_value, "todo"))?
    }
}
