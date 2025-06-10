use chumsky::{Parser, prelude::any};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{GreenExtra, codegen::mir::Mir};

struct ParserGenerator;

impl ParserGenerator {
    fn parser(mir: &Mir) -> TokenStream {
        match mir {
            Mir::Empty => Self::into_function(quote! {any().ignored()}),
            _ => todo!(),
        }
    }

    fn into_function(body: TokenStream) -> TokenStream {
        quote! {fn parser<'src, 'cache, 'interner, Err, Sy>()
            -> impl Parser<'src, Input<'src>, (), GreenExtra<'cache, 'interner, Err, Sy>>
            where
                Sy: cstree::Syntax + 'src,
                Err: chumsky::error::Error<'src, &'src str> + 'src,
                'cache: 'src,
                'interner: 'src,
                'interner: 'cache
            {
                #body
            }
        }
    }
}

#[cfg(test)]
mod test {
    use regex_syntax::ParserBuilder;

    use crate::codegen::generator::ParserGenerator;

    use super::*;

    fn normalize(s: String) -> String {
        s.split_whitespace().collect()
    }
    #[test]
    fn empty() {
        let mir = Mir::parse("").unwrap();
        let code = ParserGenerator::parser(&mir).to_string();
        assert_eq!(
            normalize(code),
            normalize(ParserGenerator::into_function(quote! {any().ignored()}).to_string())
        );
    }
}
