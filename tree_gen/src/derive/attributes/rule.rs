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
        properties::{Property, PropertyKind, Recursive},
    },
    language::{ElementError, Language, LanguageElement},
};

#[derive(Debug, From)]
pub struct Rule {
    pub dsl: DslExpr,
    pub name: Ident,
    pub parameters: HashMap<String, Ident>,
    pub is_recursive: bool,
}

impl Rule {
    pub fn new(dsl: DslExpr, name: Ident, parameters: HashMap<String, Ident>) -> Self {
        Self {
            dsl,
            name,
            parameters,
            is_recursive: false,
        }
    }

    pub fn parser_body(&self, language: &Language) -> TokenStream {
        if self.is_recursive {
            self.recursive_parser_body(language)
        } else {
            self.plain_parser_body(language)
        }
    }

    fn plain_parser_body(&self, language: &Language) -> TokenStream {
        let body = self.dsl.parser(&language.idents, &self.parameters, None);
        quote! {
            use ::tree_gen::chumsky::prelude::*;
            use tree_gen::chumksy_ext::*;
            #body
        }
    }
    fn recursive_parser_body(&self, language: &Language) -> TokenStream {
        let body = self
            .dsl
            .parser(&language.idents, &self.parameters, Some(&self.name));

        quote! {
            use ::tree_gen::chumsky::prelude::*;
            use tree_gen::chumksy_ext::*;

            recursive(|this| {
                #body
            })
        }
    }

    pub fn parser(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;
        let name = &self.name;
        let body = self.parser_body(language);

        // Collect parameters for the parser fn signature
        let param_idents: Vec<_> = self.parameters.values().collect();

        // Generate each parameter declaration
        let param_decls = param_idents.iter().map(|ident| {
            quote! {
                #ident: impl ::tree_gen::chumksy_ext::BuilderParser<'src, 'cache, 'interner, (), Err, #lang_ident> + Clone
            }
        });

        quote! {
            impl #name {
                fn parser<'src, 'cache, 'interner, Err>(
                    #(#param_decls),*
                ) -> impl ::tree_gen::chumksy_ext::BuilderParser<'src, 'cache, 'interner, (), Err, #lang_ident> + Clone
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

    fn dependencies(&self) -> std::collections::HashSet<String> {
        let mut deps = std::collections::HashSet::new();
        self.dsl.collect_deps(&mut deps);
        // strip out parameter names (they’re not real rules)
        for param in self.parameters.keys() {
            deps.remove(param);
        }
        deps
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

        // optional generic-like params: <x, y, z>
        let parameters = if input.peek(syn::Token![<]) {
            let _: syn::Token![<] = input.parse()?;
            let params: syn::punctuated::Punctuated<syn::Ident, syn::Token![,]> =
                Punctuated::parse_separated_nonempty(&input)?;
            let _: syn::Token![>] = input.parse()?;
            params
                .into_iter()
                .map(|ident| (ident.to_string(), ident))
                .collect()
        } else {
            std::collections::HashMap::new()
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
            is_recursive: false,
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

    fn build(
        &mut self,
        properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        if properties.contains(&Property::Recursive(Recursive)) {
            self.is_recursive = true;
        };
        // Insert into dependency graph
        let deps = self.dependencies();
        language.cycle_graph.add_deps(self.name.to_string(), deps);

        Ok(())
    }
}
