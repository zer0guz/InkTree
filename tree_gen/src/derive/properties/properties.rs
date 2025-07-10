


impl FromMeta<PropertyError> for PropertyKind {
    fn from_meta(meta: &Meta) -> Result<Self, PropertyError> {
        match meta {
            Meta::Path(path) => Self::from_path(path),
            Meta::List(meta_list) => Self::from_list(meta_list),
            Meta::NameValue(meta_name_value) => Self::from_name_value(meta_name_value),
        }
    }
}