use syn::Variant;

use crate::{
    attributes::{properties::SyntaxPropertyKind, SyntaxProperty}, LanguageElement, Verify
};

#[derive(Debug)]
pub struct Token {
    pub text: String,
}

impl Verify for Token {
    const ALLOWED: &[SyntaxPropertyKind] = &[];
}

impl LanguageElement for Token {
    fn codegen(&self, stream: &mut proc_macro2::TokenStream) {
        // let span = self.source.span();
        // let ident = &self.source.ident;
        // let tree = quote!{ span=>
        //     struct #ident {}
        // };
        // stream.extend(tree);
    }
}