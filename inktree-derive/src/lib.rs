use inktree::{
    Errors,
    language::{Error, build},
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

#[proc_macro_derive(SyntaxGenerator, attributes(inktree))]
pub fn syntax_generator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    derive(input).unwrap_or_else(into_compile_error).into()
}

fn derive(input: DeriveInput) -> Result<TokenStream, Errors<Error>> {
    build(input)
}

fn into_compile_error(error: Errors<Error>) -> proc_macro2::TokenStream {
    let compile_errors = error.into_compile_errors();

    quote!(#(#compile_errors)*)
}
