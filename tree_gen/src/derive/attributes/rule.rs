use std::collections::HashMap;

use chumsky::Parser;
use derive_more::From;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, MetaList, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
};

use crate::{
    derive::{
        attributes::allowed::ALLOWED_RULE,
        language::struct_def,
        parser::{DslExpr, FromMeta},
        properties::PropertyKind,
    },
    language::{ElementError, Language, LanguageElement},
};

#[derive(Debug, From)]
pub struct Rule {
    pub dsl: DslExpr,
    pub name: Ident,
    pub parameters: HashMap<String, Ident>,
}

impl Rule {
    pub fn parser_body(&self, language: &Language) -> TokenStream {
        let body = self.dsl.parser(&language.idents, &self.parameters);
        quote! {
            use ::tree_gen::chumsky::prelude::*;
            use tree_gen::chumksy_ext::*;
            #body
        }
    }

    fn parser(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;
        let name = &self.name;
        let body = self.parser_body(language);
        quote! {
            impl #name {
                fn parser<'src, 'cache, 'interner, Err>()
                -> impl ::tree_gen::chumksy_ext::BuilderParser<'src, 'cache, 'interner, (), Err, #lang_ident>
                where
                    Err: chumsky::error::Error<'src, &'src str> + 'src,

                    'cache: 'src,
                    'interner: 'cache,
                {
                    #body
                }
            }
        }
    }
}

impl FromMeta for Rule {
    fn from_list(list: &MetaList, _: Option<&Ident>) -> Result<Self, ElementError> {
        Ok(syn::parse2::<Rule>(list.tokens.clone())?)
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        // parse the rule name
        let name: Ident = input.parse()?;

        // optionally parse parameters in parentheses
        let parameters = if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            let params: Punctuated<Ident, Token![,]> =
                content.parse_terminated(Ident::parse, Token![,])?;
            params
                .into_iter()
                .map(|ident| (ident.to_string(), ident))
                .collect()
        } else {
            HashMap::new()
        };

        // expect '='
        input.parse::<Token![=]>()?;

        // everything else is your DSL rhs
        let rhs = input.parse::<proc_macro2::TokenStream>()?.to_string();
        let tokens = crate::derive::parser::dsl_lexer()
            .parse(rhs.as_str())
            .into_result()
            .map_err(|_| syn::Error::new(rhs.span(), "failed to lex DSL"))?;
        let dsl = crate::derive::parser::dsl_parser()
            .parse(&tokens)
            .into_result()
            .map_err(|_| syn::Error::new(rhs.span(), "failed to parse DSL"))?
            .into();

        Ok(Rule {
            dsl,
            name,
            parameters,
        })
    }
}

impl LanguageElement for Rule {
    fn codegen(&self, language: &Language) -> Result<TokenStream, ElementError> {
        let def_body = quote! {};
        let def = struct_def(def_body, &self.name);

        let parser = self.parser(&language);

        Ok(quote! {
            #def
            #parser
        })
    }
    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_RULE
    }

    fn name(&self) -> &Ident {
        &self.name
    }
}
