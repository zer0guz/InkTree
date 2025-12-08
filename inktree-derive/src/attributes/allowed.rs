use crate::properties::PropertyKind::{self, *};

pub static ALLOWED_NODE: &[PropertyKind] = &[Root, Ast, Recover];
pub static ALLOWED_PRATT: &[PropertyKind] = &[Root, Ast, Recover];
pub static ALLOWED_RULE: &[PropertyKind] = &[Recover];
pub static ALLOWED_TOKEN: &[PropertyKind] = &[Extra, Ast];
pub static ALLOWED_STATIC_TOKEN: &[PropertyKind] = &[OpInfix, OpPrefix, OpPostfix, Extra, Ast];
