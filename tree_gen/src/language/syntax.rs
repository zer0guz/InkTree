use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::attributes::static_token::{self, StaticToken};

pub trait Syntax: cstree::Syntax + 'static {
    const ROOT: &'static Self;
    const STATIC_TOKENS: &'static [Self];
    const NODES: &'static [Self];
    const TOKENS: &'static [Self];

    fn static_text(self) -> Option<&'static str>;

    fn from_raw(raw: cstree::RawSyntaxKind) -> Self;

    fn into_raw(self) -> cstree::RawSyntaxKind;
}
