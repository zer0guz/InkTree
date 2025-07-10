mod syntax_variant;
mod language_enum;
mod attribute_or_property;


pub(crate) use language_enum::{LanguageEnum,EnumError};
pub(crate) use syntax_variant::{VariantError,SyntaxVariant};
pub(crate) use attribute_or_property::{AttributeOrProperty};