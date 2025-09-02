use std::collections::HashSet;

use chumsky::Parser;
use derive_more::From;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
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
        let recursion_info = language.recursion_info.as_ref().expect("no recusrion info");

        let in_cycle = recursion_info.node_to_comp.contains_key(&self.name);
        let anchor_ident = format_ident!("recursion_anchor");

        // --- Collect declared generic params ---
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

        // --- If in cycle, insert anchor param ---
        if in_cycle {
            param_decls.insert(
                0,
                quote! {
                    #anchor_ident: impl ::tree_gen::chumksy_ext::BuilderParser<
                        'src, 'cache, 'interner, (), Err, #lang_ident
                    > + Clone + 'src
                },
            );
            let function = quote! {
                fn parser<'src, 'cache, 'interner, Err>()
                    -> impl ::tree_gen::chumksy_ext::BuilderParser<
                        'src, 'cache, 'interner, (), Err, #lang_ident> + Clone + 'src
                where
                    Err: chumsky::error::Error<'src, &'src str> + 'src,
                    'cache: 'src,
                    'interner: 'cache,
                {
                    use ::tree_gen::chumsky::prelude::*;
                    recursive(|#anchor_ident| {
                        #name_ident::anchored_parser(#anchor_ident)
                    })
                }
            };

            let impl_code = if is_node {
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
            };

            quote! {
                impl #name_ident {
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
                }
                #impl_code

            }
        } else {
            // acyclic rule → normal Parseable impl (still thread generics!)
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
        let def_body = quote! {};
        let def = struct_def(def_body, &self.name);

        let parser = self.parser(self.parser_body(&language), &language, false);

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
