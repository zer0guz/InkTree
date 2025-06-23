use proc_macro2::{TokenStream, TokenTree};

use crate::SyntaxVariant;

impl<'a> SyntaxVariant<'a> {
    pub(crate) fn needs_struct(&self) -> Option<TokenTree> {
        todo!("needs struct?")
    }
}
