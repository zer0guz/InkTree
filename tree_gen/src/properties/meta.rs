use proc_macro2::Span;
use snafu::Snafu;
use syn::{Meta, MetaList, MetaNameValue, Path};

use crate::{properties::PropertyError, util::span};

#[derive(Debug, Snafu)]
pub enum MetaError {
    #[snafu(display("list not supported for this property: {}", source))]
    List { source: syn::Error },
    #[snafu(display("path not supported for this property: {}", source))]
    Path { source: syn::Error },
    #[snafu(display("nameValue not supported for this property: {}", source))]
    NameValue { source: syn::Error },
}

impl MetaError {
    fn list(span: Span) -> Self {
        Self::List {
            source: syn::Error::new(span, "todo"),
        }
    }
    fn path(span: Span) -> Self {
        Self::Path {
            source: syn::Error::new(span, "todo"),
        }
    }
    fn name_value(span: Span) -> Self {
        Self::NameValue {
            source: syn::Error::new(span, "todo"),
        }
    }
}

pub trait FromMeta: Sized {
    fn from_meta(meta: Meta) -> Result<Self, PropertyError> {
        match meta {
            Meta::Path(path) => Self::from_path(path),
            Meta::List(meta_list) => Self::from_list(meta_list),
            Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value),
        }
    }

    fn from_list(list: MetaList) -> Result<Self, PropertyError> {
        Err(MetaError::list(span(list)).into())
    }

    fn from_path(path: Path) -> Result<Self, PropertyError> {
        Err(MetaError::path(span(path)).into())
    }
    fn from_name_value(name_value: MetaNameValue) -> Result<Self, PropertyError> {
        Err(MetaError::name_value(span(name_value)).into())
    }
}
