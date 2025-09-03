use std::collections::HashSet;

use chumsky::Parser;
use derive_more::From;
use proc_macro2::{Span, TokenStream};
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
        parser::{DslExpr, FromMeta, ParserCtx},
        properties::{Property, PropertyKind},
    },
    language::{ElementError, Language, LanguageElement},
};

#[derive(Debug, From)]
pub(crate) struct Rule {
    pub name: Ident,
    pub parameters: HashSet<Ident>,
    pub dsl: DslExpr,
}

impl Rule {
    pub fn new(dsl: DslExpr, name: Ident, parameters: HashSet<Ident>) -> Self {
        Self {
            name,
            parameters,
            dsl: dsl,
        }
    }
    pub fn parser_body(&self, language: &Language) -> TokenStream {
        // Always generate a stable anchor ident for this rule’s SCC
        let anchor_ident = Ident::new(
            &format!("{}_anchor", self.name.to_string().to_lowercase()),
            self.name.span(),
        );
        let ctx = ParserCtx::new(
            &language.idents,
            &self.parameters,
            language.recursion_info.as_ref().expect("no recusrion info"),
            &self.name,
            &anchor_ident,
        );

        self.dsl.parser(&ctx)
    }
    pub fn parser(&self, body: TokenStream, language: &Language, is_node: bool) -> TokenStream {
        let lang_ident = &language.ident;
        let name_ident = &self.name;
        let recursion_info = language.recursion_info.as_ref().expect("no recursion info");

        let in_cycle = recursion_info.node_to_comp.contains_key(&self.name);

        // collect declared generic params
        let mut param_decls: Vec<TokenStream> = self
            .parameters
            .iter()
            .map(|ident| {
                quote! {
                    #ident: impl ::tree_gen::chumksy_ext::BuilderParser<
                        'src, 'cache, 'interner, (), Err, #lang_ident
                    > + Clone + 'src
                }
            })
            .collect();

        if in_cycle {
            // ---------------------------
            // 1. Anchored parser signature: take deps from the SCC
            // ---------------------------
            // Get this rule's SCC index
            let comp_idx = recursion_info.node_to_comp[&self.name];
            let comp: Vec<Ident> = recursion_info.components[comp_idx]
                .iter()
                .map(|ident| {
                    Ident::new(ident.to_string().to_lowercase().as_str(), Span::call_site())
                })
                .collect();

            // Build params for each peer dependency in SCC (excluding self)
            let dep_params: Vec<TokenStream> = comp
                .iter()
                .filter(|n| *n != &self.name)
                .map(|peer| {
                    let peer_ident = peer;
                    quote! {
                        #peer_ident: impl ::tree_gen::chumksy_ext::BuilderParser<
                            'src, 'cache, 'interner, (), Err, #lang_ident
                        > + Clone + 'src
                    }
                })
                .collect();

            // prepend those to declared generics
            param_decls.splice(0..0, dep_params);

            let anchored_fn = quote! {
                fn anchored_parser<'src, 'cache, 'interner, Err>(
                    #(#param_decls),*
                ) -> impl ::tree_gen::chumksy_ext::BuilderParser<
                    'src, 'cache, 'interner, (), Err, #lang_ident
                > + Clone
                where
                    Err: chumsky::error::Error<'src, &'src str> + 'src,
                    'cache: 'src,
                    'interner: 'cache,
                {
                    #body
                }
            };

            // ---------------------------
            // 2. Parser impl using Recursive::declare/define
            // ---------------------------
            let parser_fn = quote! {
                fn parser<'src, 'cache, 'interner, Err>()
                    -> impl ::tree_gen::chumksy_ext::BuilderParser<
                        'src, 'cache, 'interner, (), Err, #lang_ident> + Clone + 'src
                where
                    Err: chumsky::error::Error<'src, &'src str> + 'src,
                    'cache: 'src,
                    'interner: 'cache,
                {
                    use ::tree_gen::chumsky::prelude::*;
                    use ::tree_gen::chumsky::recursive::Recursive;

                    // one declare for each SCC member
                    #( let mut #comp = Recursive::declare(); )*

                    // define each
                    #( #comp.define(#comp::anchored_parser(/* pass deps here */)); )*

                    // return this one’s handle
                    #name_ident
                }
            };

            let impl_code = if is_node {
                quote! {
                    impl ::tree_gen::Parseable for #name_ident {
                        type Syntax = #lang_ident;
                        #parser_fn
                    }
                }
            } else {
                quote! {
                    impl #name_ident {
                        #parser_fn
                    }
                }
            };

            quote! {
                impl #name_ident {
                    #anchored_fn
                }
                #impl_code
            }
        } else {
            // acyclic rule (unchanged)
            let function = quote! {
                fn parser<'src, 'cache, 'interner, Err>(#(#param_decls),*)
                    -> impl ::tree_gen::chumksy_ext::BuilderParser<
                            'src, 'cache, 'interner, (), Err, #lang_ident> + Clone + 'src
                        where
                            Err: chumsky::error::Error<'src, &'src str> + 'src,
                            'cache: 'src,
                            'interner: 'cache,
                        {
                            use ::tree_gen::chumsky::prelude::*;
                            use tree_gen::chumksy_ext::*;
                            #body
                        }
            };
            if is_node {
                quote! {
                    impl ::tree_gen::Parseable for #name_ident {
                        type Syntax = #lang_ident;
                        #function
                    }
                }
            } else {
                quote! {
                    impl #name_ident {
                        #function
                    }
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

        // optional generic-like params: <x, y, z>
        let parameters = if input.peek(syn::Token![<]) {
            let _: syn::Token![<] = input.parse()?;
            let params: Punctuated<syn::Ident, syn::Token![,]> =
                Punctuated::parse_separated_nonempty(&input)?;
            let _: syn::Token![>] = input.parse()?;
            params.into_iter().collect()
        } else {
            HashSet::new()
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
        let ident = &self.name;
        let lang_ident = &language.ident;

        let parser = self.parser_body(&language);

        Ok(quote! {
            rule!(#lang_ident::#ident,[],{#parser});
        })
    }
    fn allowed(&self) -> &'static [PropertyKind] {
        ALLOWED_RULE
    }

    fn name(&self) -> &Ident {
        &self.name
    }

    fn build(
        &self,
        _properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        // Insert into dependency graph

        let self_handle = language.elements.next_handle();
        language.rules.push(self_handle);

        let mut deps = std::collections::HashSet::new();
        self.dsl.collect_deps(&self.parameters, &mut deps);
        for param in &self.parameters {
            deps.remove(param);
        }

        language.cycle_graph.add_rule(self.name.clone(), deps);

        Ok(())
    }
}
