use std::collections::HashSet;

use chumsky::Parser;
use derive_more::From;
use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    Ident, MetaList, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
};

use crate::{
    LowerCtx, Shape,
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
    pub parameters: Vec<Ident>,
    pub dsl: DslExpr,
}

impl Rule {
    pub fn new(dsl: DslExpr, name: Ident, parameters: Vec<Ident>) -> Self {
        Self {
            name,
            parameters,
            dsl: dsl,
        }
    }
    pub fn parser_body(&self, language: &Language) -> TokenStream {
        let recursion_info = language.recursion_info.as_ref().expect("no recursion info");

        let anchored = if let Some(&comp_idx) = recursion_info.node_to_comp.get(&self.name) {
            let comp = &recursion_info.components[comp_idx];

            let deps = recursion_info
                .adj
                .get(&self.name)
                .cloned()
                .unwrap_or_default();

            deps.into_iter().filter(|d| comp.contains(d)).collect()
        } else {
            HashSet::new()
        };

        let ctx = ParserCtx::new(&self.parameters, anchored);

        self.dsl.parser(&ctx)
    }
    pub fn parser(&self, body: TokenStream, language: &Language, is_node: bool) -> TokenStream {
        let lang_ident = &language.ident;
        let name_ident = &self.name;
        let recursion_info = language.recursion_info.as_ref().expect("no recursion info");

        let in_cycle = recursion_info.node_to_comp.contains_key(&self.name);

        // collect declared params
        let param_idents: Vec<_> = self.parameters.iter().collect();

        if in_cycle {
            // build peer + anchor info from SCC
            let comp_idx = recursion_info.node_to_comp[&self.name];
            let peers = &recursion_info.components[comp_idx];
            let decls = peers
                .iter()
                .map(|peer| {
                    Ident::new(
                        &format!("{}_decl", peer.to_string().to_lowercase()),
                        Span::call_site(),
                    )
                })
                .collect_vec();

            let used_anchors = recursion_info
                .adj
                .get(&self.name)
                .into_iter()
                .flatten()
                .filter(|peer| peers.contains(peer)) // only keep peers in this SCC
                .map(|peer| Ident::new(&peer.to_string().to_lowercase(), Span::call_site()))
                .collect_vec();

            let defines = peers
                .iter()
                .map(|peer| {
                    // find edges of this peer inside SCC
                    let used_edges = recursion_info
                        .adj
                        .get(peer)
                        .into_iter()
                        .flatten()
                        .filter_map(|edge| {
                            if peers.contains(edge) {
                                Some(Ident::new(
                                    &format!("{}_decl", edge.to_string().to_lowercase()),
                                    Span::call_site(),
                                ))
                            } else {
                                None
                            }
                        });

                    let peer_decl = Ident::new(
                        &format!("{}_decl", peer.to_string().to_lowercase()),
                        Span::call_site(),
                    );
                    // wrap as Peer( …anchors… )
                    quote!( #peer_decl.define(#peer::anchored_parser(#(#used_edges.clone()),* )) )
                })
                .collect_vec();

            let return_value = Ident::new(
                &format!("{}_decl", self.name.to_string().to_lowercase()),
                Span::call_site(),
            );

            let wrapper = quote! {
                use ::inktree::chumsky::recursive::*;

                // one declare for each SCC member
                #( let mut #decls = Recursive::declare();)*
                // define each SCC member with its specific anchors
                #( #defines;)*

                // return this one’s handle
                #return_value

            };

            if is_node {
                quote! {
                    #[derive(Debug)]
                    pub struct #name_ident;
                    impl #name_ident {
                        inktree::make_anchored_parser!(#lang_ident, [#(#used_anchors),*], [#(#param_idents),*], { #body });
                        //inktree::make_recursive_parser!(#lang_ident, #name_ident, [#(#peer_anchors),*]);
                    }
                    inktree::parseable!(#lang_ident::#name_ident,  [#(#param_idents),*],{#wrapper});
                }
            } else {
                quote! {
                    #[derive(Debug)]
                    pub struct #name_ident;
                    impl #name_ident {
                        inktree::make_anchored_parser!(#lang_ident, [#(#used_anchors),*], [#(#param_idents),*], { #body });
                        inktree::make_parser!(#lang_ident,[#(#param_idents),*], { #wrapper });
                        //inktree::make_recursive_parser!(#lang_ident, #name_ident, [#(#peer_anchors),*]);
                    }
                }
            }
        } else {
            if is_node {
                quote! {
                    #[derive(Debug)]
                    pub struct #name_ident;
                    inktree::parseable!(#lang_ident::#name_ident, [#(#param_idents),*],#body);
                }
            } else {
                quote! {
                    #[derive(Debug)]
                    pub struct #name_ident;
                    impl #name_ident {
                        inktree::make_parser!(#lang_ident,[#(#param_idents),*],{#body});

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
        let parser = self.parser(self.parser_body(&language), &language, false);

        Ok(quote! {
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
        _properties: &Vec<Property>,
        language: &mut Language,
    ) -> Result<(), ElementError> {
        // Collect in the original traversal order.
        let mut deps: Vec<Ident> = Vec::new();
        self.dsl.collect_deps(&self.parameters, &mut deps);

        // Remove formal parameters.
        deps.retain(|d| !self.parameters.contains(d));

        // Stable de-dup (keep first occurrence).
        let mut seen = std::collections::HashSet::<Ident>::new();
        deps.retain(|d| seen.insert(d.clone()));

        language.cycle_graph.add_rule(self.name.clone(), &deps);
        Ok(())
    }

    fn ast_shape(&self, lang: &Language) -> Option<Shape> {
        let mut cx = LowerCtx::new(lang);
        cx.lower_rule_dsl(&self.name, &self.dsl)
    }
}
