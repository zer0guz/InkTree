use snafu::{ResultExt, Snafu};
use syn::{Meta, MetaList, MetaNameValue, Path};

use crate::attributes::{AttributeError, SyntaxAttribute};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum MetaError {
    #[snafu(display("list not supported for this property: {}", source))]
    List { source: syn::Error },
    #[snafu(display("path not supported for this property: {}", source))]
    Path { source: syn::Error },
    #[snafu(display("nameValue not supported for this property: {}", source))]
    NameValue { source: syn::Error },
    #[snafu(display("the type of this Value not supported for this property: {}", source))]
    Value { source: syn::Error },
}

pub trait FromMeta: Sized {
    fn from_meta(meta: &Meta) -> Result<SyntaxAttribute, AttributeError> {
        match meta {
            Meta::Path(path) => Self::from_path(path),
            Meta::List(meta_list) => Self::from_list(meta_list),
            Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value),
        }
    }

    fn from_list(list: &MetaList) -> Result<SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(list, "todo")).context(ListSnafu)?
    }

    fn from_path(path: &Path) -> Result<SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(path, "todo")).context(PathSnafu)?
    }
    fn from_name_value(name_value: &MetaNameValue) -> Result<SyntaxAttribute, AttributeError> {
        Err(syn::Error::new_spanned(name_value, "todo")).context(NameValueSnafu)?
    }
}
