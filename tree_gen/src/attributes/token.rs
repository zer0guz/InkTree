use syn::Variant;

use crate::{
    attributes::{properties::SyntaxPropertyKind, SyntaxProperty}, error::Errors, LanguageElement, SemanticError
};

#[derive(Debug)]
pub struct Token {
    pub text: String,
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
    
    fn allowed(&self) ->  &'static[SyntaxPropertyKind] {
        todo!()
    }
    
    fn build(&mut self,properties:Vec<SyntaxProperty>,source: Variant){
        todo!()
    }
    

    
}