use chumsky::prelude::just;
use chumsky::prelude::*;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::{Builder, language::syntax::Syntax};




pub fn struct_def(body:TokenStream,ident:&Ident) -> TokenStream {
    quote! {
        struct #ident {
            #body
        }
    }
}
