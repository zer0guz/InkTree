use proc_macro2::TokenStream;
use quote::quote;

use crate::language::mir::Mir;

struct ParserGenerator;

impl ParserGenerator {
    fn parser(mir: &Mir) -> TokenStream {
        match mir {
            Mir::Empty => Self::into_impl(quote! {any().ignored()}),
            _ => todo!("generator"),
        }
    }

    fn into_impl(body: TokenStream) -> TokenStream {
        quote! {
                #body

        }
    }
}

#[cfg(test)]
mod test {

    use crate::language::generator::ParserGenerator;

    use super::*;

    fn normalize(s: String) -> String {
        s.split_whitespace().collect()
    }
    #[test]
    fn empty() {
        let mir = Mir::parse("").unwrap();
        let code = ParserGenerator::parser(&mir).to_string();
        // assert_eq!(
        //     normalize(code),
        //     normalize(ParserGenerator::into_function(quote! {any().ignored()}).to_string())
        // );
    }
}
