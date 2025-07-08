use derive_more::From;
use strum::{EnumDiscriminants, EnumString};
use syn::Meta;

use crate::language::attributes::AttributeError;

#[derive(Debug, PartialEq, Eq, EnumDiscriminants, From)]
#[strum(serialize_all = "kebab-case")]
#[strum_discriminants(vis(pub))]
#[strum_discriminants(name(SyntaxPropertyKind), derive(EnumString))]
pub enum SyntaxProperty {
    None(()),
}

impl SyntaxPropertyKind {
    pub fn from_meta(self, _: &Meta) -> Result<SyntaxProperty, AttributeError> {
        todo!("from meta (property)")
    }
}
