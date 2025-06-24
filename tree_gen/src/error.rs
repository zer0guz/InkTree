use std::fmt::Display;

use proc_macro2::TokenStream;
use snafu::{AsErrorSource, Snafu};

use crate::{SemanticError, SyntaxError};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
pub enum Error {
    #[snafu(display("Error during codegen"))]
    #[snafu(context(false))]
    CodeGen { source: Errors<SemanticError> },
    #[snafu(display("Error while parsing   {}", source))]
    #[snafu(context(false))]
    Parse { source: Errors<SyntaxError> },
}

impl Error {
    pub fn into_compile_errors(self) -> Vec<proc_macro2::TokenStream> {
        fn find_source(mut error: &(dyn std::error::Error + 'static)) -> TokenStream {
            while !error.is::<syn::Error>() {
                error = error.source().unwrap()
            }
            let span = error.downcast_ref::<syn::Error>().unwrap().span();
            syn::Error::new(span, format!("{}", error)).into_compile_error()
        }
        let errors = match self {
            Error::CodeGen { source } => source.into_compile_errors(),
            Error::Parse { source } => source
                .0
                .into_iter()
                .map(|err| find_source(err.as_error_source()))
                .collect(),
        };

        errors
    }
}

#[derive(Debug)]
pub struct Errors<E>(pub Vec<E>)
where
    E: std::error::Error;

impl<E> Errors<E>
where
    E: std::error::Error + 'static,
{
    pub fn into_compile_errors(self) -> Vec<proc_macro2::TokenStream> {
        fn find_source(mut error: &(dyn std::error::Error + 'static)) -> TokenStream {
            while !error.is::<syn::Error>() {
                error = error.source().unwrap()
            }
            let span = error.downcast_ref::<syn::Error>().unwrap().span();
            syn::Error::new(span, format!("{}", error)).into_compile_error()
        }
        self.0
            .into_iter()
            .map(|err| find_source(err.as_error_source()))
            .collect()
    }
    pub fn convert_errors<T: snafu::Error + std::convert::From<E>>(self) -> Errors<T> {
        self.0.into_iter().map(Into::into).collect()
    }
}

impl<T> From<T> for Errors<T>
where
    T: snafu::Error,
{
    fn from(value: T) -> Self {
        Self(vec![value])
    }
}

impl<T, U> FromIterator<T> for Errors<U>
where
    U: snafu::Error,
    U: From<T>,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter.into_iter().map(|val| U::from(val))))
    }
}

impl<E> Into<Vec<E>> for Errors<E>
where
    E: std::error::Error,
{
    fn into(self) -> Vec<E> {
        self.0
    }
}

impl<E> Display for Errors<E>
where
    E: std::error::Error,
{
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("format")
    }
}

impl<E: std::error::Error> From<Vec<E>> for Errors<E> {
    fn from(value: Vec<E>) -> Self {
        Self(value)
    }
}

impl<E: std::error::Error> std::error::Error for Errors<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}
