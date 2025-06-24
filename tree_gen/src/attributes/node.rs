use proc_macro2::TokenStream;
use syn::Ident;

use crate::{
    LanguageElement, attributes::{SyntaxProperty, SyntaxPropertyKind},
};

#[derive(Debug)]
pub struct Node {
    pub text: String,
}

impl LanguageElement for Node {
    fn allowed(&self) -> &'static [SyntaxPropertyKind] {
        todo!()
    }
    
    fn codegen(&self,_: &Vec<SyntaxProperty>,_: &Ident,_: &mut TokenStream) {
        todo!()
    }

}
