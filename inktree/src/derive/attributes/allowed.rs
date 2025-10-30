use crate::derive::properties::PropertyKind;

use PropertyKind::*;

pub static ALLOWED_NODE: &[PropertyKind] = &[Root, Ast];
pub static ALLOWED_PRATT: &[PropertyKind] = &[Root, Ast];
pub static ALLOWED_RULE: &[PropertyKind] = &[];
pub static ALLOWED_TOKEN: &[PropertyKind] = &[Extra, Ast];
pub static ALLOWED_STATIC_TOKEN: &[PropertyKind] = &[OpInfix, OpPrefix, OpPostfix, Extra, Ast];
