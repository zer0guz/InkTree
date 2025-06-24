use syn::Variant;

use crate::{attributes::{SyntaxProperty, SyntaxPropertyKind}, error::Errors, LanguageElement, SemanticError};


#[derive(Debug)]
pub struct Node {
    pub text: String,
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
    
    fn allowed(&self) ->  &'static[SyntaxPropertyKind] {
        todo!()
    }
    
    fn build(&mut self,properties:Vec<SyntaxProperty>,source: Variant) {
        todo!()
    }
    
    
    
}