use snafu::{AsErrorSource, Snafu};

use crate::{ParseError, codegen::CodegenError};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Error during codegen: {}", source))]
    #[snafu(context(false))]
    CodeGen { source: CodegenError },
    #[snafu(display("Error while parsing   {}", source))]
    #[snafu(context(false))]
    Parse { source: ParseError },
}

impl Error {
    pub fn into_compile_error(&self) -> proc_macro2::TokenStream {
        let mut error = self.as_error_source();
        while !error.is::<syn::Error>() {
            error = error.source().unwrap()
        }
        let span = error.downcast_ref::<syn::Error>().unwrap().span();
        syn::Error::new(span, format!("{}", self)).into_compile_error()
    }
}
