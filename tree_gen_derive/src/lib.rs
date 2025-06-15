use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};
use tree_gen::{Error, SyntaxEnum};

#[proc_macro_derive(SyntaxGenerator, attributes(tree_gen))]
pub fn syntax_generator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    derive(input).unwrap_or_else(into_compile_error).into()
}

fn derive(input: DeriveInput) -> Result<TokenStream, Vec<Error>> {
    SyntaxEnum::from_input(input)?.codegen()
}

fn into_compile_error(errors: Vec<Error>) -> proc_macro2::TokenStream {
    let compile_errors = errors
        .iter()
        .map(Error::into_compile_error);

    quote!(#(#compile_errors)*)
}
