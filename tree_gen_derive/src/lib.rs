use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};
use tree_gen::{Errors, Language, LanguageError};

#[proc_macro_derive(SyntaxGenerator, attributes(tree_gen))]
pub fn syntax_generator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    derive(input).unwrap_or_else(into_compile_error).into()
}

fn derive(input: DeriveInput) -> Result<TokenStream, Errors<LanguageError>> {
    Language::from_input(input)?.codegen()
}

fn into_compile_error(error: Errors<LanguageError>) -> proc_macro2::TokenStream {
    let compile_errors = error.into_compile_errors();

    quote!(#(#compile_errors)*)
}
