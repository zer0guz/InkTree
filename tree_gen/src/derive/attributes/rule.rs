use chumsky::Parser;
use derive_more::From;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;
use snafu::Snafu;
use syn::{
    Ident, MetaList, Path, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
};

use crate::{derive::{
    attributes::allowed::ALLOWED_RULE, language::struct_def, parser::{dsl_lexer, dsl_parser, DslExpr, FromMeta}, properties::PropertyKind
}, language::{ElementError, Language, LanguageElement}};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum RuleError {
    #[snafu(display("todo"))]
    #[snafu(context(false))]
    Parse { source: syn::Error },
}

#[derive(Debug, From)]
pub struct Rule {
    dsl: DslExpr,
    name: Ident,
    parameters: Vec<Ident>,
}

impl Rule {
    pub fn new(dsl: DslExpr, name: Ident, parameters: Vec<Ident>) -> Self {
        Self {
            dsl,
            name,
            parameters,
        }
    }

    pub fn from_string(input: String) -> Result<Self, RuleError> {
        let (lhs, rhs) = input
            .split('=')
            .collect_tuple()
            .expect("TODO = sign in rule error");

        let (name, parameters) = if let Ok(path) = syn::parse_str::<Path>(lhs) {
            (path.require_ident()?.clone(), vec![])
        } else if let Ok(list) = syn::parse_str::<MetaList>(lhs) {
            let ident = list.path.require_ident()?.clone();
            let params = list
                .parse_args_with(Punctuated::<Ident, Comma>::parse_terminated)?
                .into_iter()
                .collect();
            (ident, params)
        } else {
            todo!()
        };

        let tokens = dsl_lexer().parse(rhs).into_result().expect("lexer error todo");
        let dsl = dsl_parser().parse(&tokens).into_result().expect("lexer error todo").into();

        Ok(Self {
            dsl,
            name: name,
            parameters,
        })
    }

    pub fn parser_body(&self, language: &Language) -> TokenStream {
        let body = self.dsl.parser(&language.idents);
        quote! {
            use ::tree_gen::chumsky::prelude::*;
            use tree_gen::chumksy_ext::*;
            #body
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
        let parameters: Vec<Ident> = if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            let params: Punctuated<Ident, Token![,]> =
                content.parse_terminated(Ident::parse, Token![,])?;
            params.into_iter().collect()
        } else {
            Vec::new()
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

        let impl_code = self.parser_body(&language);

        Ok(quote! {
            #def
            #impl_code
        })
    }
    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_RULE
    }

    fn name(&self) -> &Ident {
        &self.name
    }
}
