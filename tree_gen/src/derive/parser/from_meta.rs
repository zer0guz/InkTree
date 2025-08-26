use snafu::ResultExt;
use syn::{Ident, Meta, MetaList, MetaNameValue, Path};

use crate::language::{ElementError, MetaSnafu};


pub trait FromMeta: Sized {
    fn from_meta(meta: &Meta,name: Option<&Ident>) -> Result<Self, ElementError> {
        match meta {
            Meta::Path(path) => Self::from_path(path,name),
            Meta::List(meta_list) => Self::from_list(meta_list,name),
            Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value,name),
        }
    }

    fn from_list(list: &MetaList,_: Option<&Ident>) -> Result<Self, ElementError> {
        Err(syn::Error::new_spanned(list, "from_meta list not supported for this thing todo")).context(MetaSnafu)?
    }

    fn from_path(path: &Path,_: Option<&Ident>) -> Result<Self, ElementError> {
        Err(syn::Error::new_spanned(path, "from_meta path not supported for this thing todo")).context(MetaSnafu)?
    }
    fn from_name_value(name_value: &MetaNameValue,_: Option<&Ident>) -> Result<Self, ElementError> {
        Err(syn::Error::new_spanned(name_value, "from_meta name_value not supported for this thing todo")).context(MetaSnafu)?
    }
}
