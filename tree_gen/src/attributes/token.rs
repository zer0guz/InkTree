use proc_macro2::TokenStream;
use syn::Ident;

use crate::{
    attributes::{properties::SyntaxPropertyKind, SyntaxProperty}, LanguageElement
};

#[derive(Debug)]
pub struct Token {
    pub text: String,
}

impl LanguageElement for Token {
    fn allowed(&self) ->  &'static[SyntaxPropertyKind] {
        todo!()
    }
    
    fn codegen(&self,_: &Vec<SyntaxProperty>,_: &Ident,_: &mut TokenStream) {
        todo!()
    }
    
}