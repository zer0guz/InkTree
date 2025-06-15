mod error;
mod generator;
mod mir;
//mod implementations;

pub use error::CodegenError;
use proc_macro2::TokenStream;

use crate::{Error, SyntaxEnum};

impl SyntaxEnum {
    pub fn codegen(self) -> Result<TokenStream, Vec<Error>> {
        todo!()
    }
}
