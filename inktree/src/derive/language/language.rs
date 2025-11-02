use crate::{
    Ast, derive::{attributes::SyntaxAttributeKind, properties::OperatorKind}, language::{
        element,
        rule_graph::{RecursionInfo, RuleGraph},
    }, util::{Handle, Pool}
};
use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream;
use snafu::{ResultExt, Snafu};
use strum::IntoDiscriminant;
use syn::{DeriveInput, Ident, parse::Parse};

use quote::quote;

use crate::{
    derive::{
        attributes::SyntaxAttribute,
        language::{Element, ElementError, LanguageElement},
        properties::Operator,
    },
    error::Errors,
    util::IteratorExt,
};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display(
        "attributes have to be provided by placing them into inktree() SCOPE TODO: {}",
        source
    ))]
    Data { source: syn::Error },

    #[snafu(display(
        "attributes have to be provided by placing them into inktree() SCOPE TODO: {}",
        source
    ))]
    Repr { source: syn::Error },

    #[snafu(context(false))]
    Custom { source: syn::Error },

    #[snafu(display(
        "attributes have to be provided by placing them into inktree() SCOPE TODO: {}",
        source
    ))]
    #[snafu(context(false))]
    Element { source: ElementError },
}


#[derive(Debug)]
pub(crate) struct Language {
    pub element_pool: Pool<Element>,
    pub ident: Ident,
    pub operators: Vec<Operator>,
    pub root: Option<Handle<Element>>,
    pub cycle_graph: RuleGraph,
    pub recursion_info: Option<RecursionInfo>,
    pub extras: Vec<Ident>,
    pub idents: HashMap<Ident, Handle<Element>>,
}

impl Language {
    fn handle_element(&mut self, mut element: Element) -> Result<(), Errors<Error>> {
        let name = element.attribute.name().clone();
        element.build(self).map_err(Errors::map_errors)?;
        let handle = self.element_pool.push(element);
        self.idents.insert(name, handle);

        return Ok(());
    }

    fn syntax_impl(&self) -> TokenStream {
        let (static_texts, parsers): (Vec<_>, Vec<_>) = self
            .element_pool
            .iter()
            .filter(|element| element.attribute.discriminant() != SyntaxAttributeKind::Rule)
            .map(|variant| {
                let ident = &variant.attribute.name();
                let static_text = match variant.attribute {
                    SyntaxAttribute::StaticToken(ref static_token) => {
                        let text = &static_token.text;
                        quote! {
                            #ident => Some(#text),
                        }
                    }
                    _ => quote! {
                        #ident => None,
                    },
                };
                let lang_ident = &self.ident;
                let parser = quote! {
                    #lang_ident::#ident => #ident::parser().boxed(),
                };
                (static_text, parser)
            })
            .collect();
        let ident = &self.ident;
        let variant_count = self.element_pool.len() as u32;
        let root_ident = self.element_pool[self.root.expect("root existence already validated")]
            .attribute
            .name();
        quote! {
            impl ::inktree::cstree::Syntax for #ident {
                fn from_raw(raw: inktree::cstree::RawSyntaxKind) -> Self {
                    <Self as ::inktree::Syntax>::from_raw(raw)
                }

                fn into_raw(self) -> inktree::cstree::RawSyntaxKind {
                    <Self as ::inktree::Syntax>::into_raw(self)
                }

                fn static_text(self) -> Option<&'static str> {
                    <Self as ::inktree::Syntax>::static_text(self)
                }
            }

            impl ::inktree::Syntax for #ident {
                type Root = #root_ident;
                fn from_raw(raw: ::inktree::cstree::RawSyntaxKind) -> Self {
                    assert!(raw.0 < #variant_count, "Invalid raw syntax kind: {}", raw.0);
                    // Safety: discriminant is valid by the assert above
                    unsafe { ::std::mem::transmute::<u32, #ident>(raw.0) }
                }

                fn into_raw(self) -> ::inktree::cstree::RawSyntaxKind {
                    ::inktree::cstree::RawSyntaxKind(self as u32)
                }

                fn static_text(self) -> ::core::option::Option<&'static str> {
                    use #ident::*;
                    match self {
                        #( #static_texts )*
                        _ => unreachable!()
                    }
                }

                fn parser<'src, 'cache, 'interner,'borrow,'extra, Err>(
                    self,
                ) -> impl ::inktree::chumsky_ext::BuilderParser<'src, 'cache, 'interner,'borrow, (), Err, Self> + Clone + 'extra
                where
                    Err: ::inktree::chumsky::error::Error<'src, &'src str> + 'extra,
                    'interner: 'cache,
                    'borrow: 'interner,
                    'src: 'extra,
                    'cache: 'extra,

                {
                    use ::inktree::chumsky::prelude::*;
                    use ::inktree::Parseable;
                    match self {
                        #( #parsers )*
                    }
                }
            }
        }
    }

    fn token_sink(&self) -> TokenStream {
        let lang_ident = &self.ident;
        let extras = &self.extras;

        quote! {
            ::inktree::make_sink!(#lang_ident, [ #( #extras ),* ]);
        }
    }

    fn codegen(&mut self) -> Result<TokenStream, Errors<Error>> {
        if self
            .recursion_info
            .as_ref()
            .expect("bug")
            .left_recursive
            .len()
            > 0
        {
            todo!("REEEE FIX YOUR LEFT RECURSION")
        }

        if self.root.is_none() {
            Err(syn::Error::new_spanned(&self.ident, "no root todo text")).map_err(Error::from)?;
        }

        let mut stream = quote! {
            use inktree::Parseable;
        };

        if !self.extras.is_empty() {
            let sink = self.token_sink();
            stream.extend(quote! {
                #sink
            });
        }

        stream.extend(self.syntax_impl());

        let variants_code = self
            .element_pool
            .iter()
            .map(|variant| Ok(variant.codegen(&self)?))
            .collect_either()?;

        stream.extend(variants_code);

        stream.extend(self.pratt_codegen());

        stream.extend(self.ast_codegen());

        Ok(stream)
    }

    fn pratt_codegen(&self) -> TokenStream {
        let mut prefix_ops = Vec::new();
        let mut infix_ops = Vec::new();
        let mut postfix_ops = Vec::new();

        for op in &self.operators {
            let tokens = op.pratt_op();
            match op.kind {
                OperatorKind::Prefix(_) => prefix_ops.push(tokens),
                OperatorKind::Infix(_) => infix_ops.push(tokens),
                OperatorKind::Postfix(_) => postfix_ops.push(tokens),
            }
        }

        quote! {
            ::inktree::define_pratt_ext!(
                TestLang,
                prefix: [ #( #prefix_ops, )* ],
                infix: [ #( #infix_ops, )* ],
                postfix: [ #( #postfix_ops, )* ]
            );
        }
    }

    pub fn ast_codegen(&mut self) -> TokenStream {
       
        let root = &self.element_pool[self.root.expect("root verified")];

        let name = root.attribute.name().clone();

        Ast::build_from_root(self,&name).codegen(&self)

        // root
        //     .attribute
        //     .ast_shape(&self)
        //     .
        //     .simplify()
        //     .codegen(&self.ident);

        
    }


}

pub fn build(input: DeriveInput) -> Result<TokenStream, Errors<Error>> {
    let mut language = Language {
        element_pool: Pool::with_capacity(input.attrs.len() as u32),
        ident: input.ident.clone(),
        operators: vec![],
        root: None,
        cycle_graph: RuleGraph::new(),
        recursion_info: None,
        idents: HashMap::new(),
        extras: vec![],
    };

    let mut repr = vec![];

    input
        .attrs
        .iter()
        .filter(|attr| {
            if attr.path().is_ident("repr") {
                repr.push(*attr);
            }
            attr.path().is_ident("inktree")
        })
        .map(|attribute| {
            language.handle_element(Element::from_attribute(&attribute).map_err(Error::from)?)
        })
        .collect_either_flatten()?;

    let repr_inner = repr
        .into_iter()
        .next()
        .ok_or_else(|| syn::Error::new_spanned(&input, "todo text repr"))
        .context(ReprSnafu)?
        .parse_args_with(Ident::parse)
        .context(ReprSnafu)?;

    if !(repr_inner == "u32") {
        return Err(syn::Error::new_spanned(repr_inner, "todo text repr2")).context(ReprSnafu)?;
    };

    let syn::Data::Enum(syntax) = &input.data else {
        Err(syn::Error::new_spanned(&input, "oups")).context(DataSnafu)?
    };

    syntax
        .variants
        .iter()
        .map(Element::from_variant)
        .collect_either_flatten_into()?
        .into_iter()
        .flatten()
        .map(|element| language.handle_element(element))
        .collect_either_flatten()?;

    let rules = language
        .idents
        .iter()
        .map(|(_, handle)| {
            let element = &language.element_pool[*handle];
            (element.attribute.name().clone(), element)
        })
        .collect();

    language.recursion_info = Some(language.cycle_graph.into_recursive_info(&rules));

    language.codegen()
}
