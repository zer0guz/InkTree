use syn::Variant;

use crate::{
    attributes::{properties::{SyntaxProperty, SyntaxPropertyKind}, StaticToken}, error::Errors, LanguageElement, SemanticError, Verify
};

#[derive(Debug)]
pub struct Node {
    pub text: String,
}

impl Verify for Node {
    const ALLOWED: &[SyntaxPropertyKind] = &[];
}


impl LanguageElement for Node {
    fn codegen(&self, stream: &mut proc_macro2::TokenStream) {
        // let span = self.source.span();
        // let ident = &self.source.ident;
        // let tree = quote!{ span=>
        //     struct #ident {}
        // };
        // stream.extend(tree);
    }
}