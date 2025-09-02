use crate::derive::properties::PropertyKind;

use PropertyKind::*;

pub static ALLOWED_NODE: &[PropertyKind] = &[Root];
pub static ALLOWED_PRATT: &[PropertyKind] = &[Root];
pub static ALLOWED_RULE: &[PropertyKind] = &[];
pub static ALLOWED_TOKEN: &[PropertyKind] = &[Whitespace, Keyword];
pub static ALLOWED_STATIC_TOKEN: &[PropertyKind] = &[OpInfix, OpPrefix, OpPostfix, Whitespace];
