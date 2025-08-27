use std::collections::HashMap;

use chumsky::extra::Err;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::chumsky::prelude::*;

#[derive(Debug, PartialEq)]
pub enum DslExpr {
    Just(String),
    Seq(Vec<DslExpr>),
    Opt(Box<DslExpr>),
    Star(Box<DslExpr>),
    Plus(Box<DslExpr>),
    Alt(Box<DslExpr>, Box<DslExpr>),
    Call { name: String, args: Vec<DslExpr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DslToken {
    Ident(String),
    Question,
    Star,
    Plus,
    Pipe,
    LParen,
    RParen,
    Lt,    // <
    Gt,    // >
    Comma, // ,
}

#[derive(Debug, Clone, Copy)]
enum UnOp {
    Opt,
    Star,
    Plus,
}

impl DslExpr {
    pub fn parser(
        &self,
        idents: &HashMap<String, Ident>,
        parameters: &HashMap<String, Ident>,
        recursive_self_ident: Option<&Ident>, // NEW: which rule are we expanding for?
    ) -> TokenStream {
        match self {
            DslExpr::Just(text) => {
                if let Some(me) = recursive_self_ident {
                    if text == &me.to_string() {
                        return quote! { this };
                    }
                }

                if let Some(ident) = idents.get(text) {
                    quote! { #ident::parser() }
                } else if let Some(ident) = parameters.get(text) {
                    quote! { #ident.clone() }
                } else {
                    todo!("todo unknown symbol")
                }
            }
            DslExpr::Seq(exprs) => {
                let folded = exprs
                    .iter()
                    .map(|e| Self::parser(e, idents, parameters, recursive_self_ident))
                    .fold(quote! {}, |acc, p| {
                        if acc.is_empty() {
                            p
                        } else {
                            quote! { #acc.then(#p) }
                        }
                    });
                quote! { #folded.ignored() }
            }
            DslExpr::Opt(inner) | DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                let p = Self::parser(inner, idents, parameters, recursive_self_ident);
                match self {
                    &DslExpr::Opt(_) => quote! { #p.or_not() },
                    &DslExpr::Star(_) => quote! { #p.repeated() },
                    &DslExpr::Plus(_) => quote! { #p.repeated().at_least(1) },
                    _ => unreachable!(),
                }
            }
            DslExpr::Alt(a, b) => {
                let pa = Self::parser(a, idents, parameters, recursive_self_ident);
                let pb = Self::parser(b, idents, parameters, recursive_self_ident);
                quote! { #pa.or(#pb) }
            }
            DslExpr::Call { name, args } => {
                if let Some(me) = recursive_self_ident {
                    if name == &me.to_string() {
                        // recursive call
                        let arg_tokens = args
                            .iter()
                            .map(|a| a.parser(idents, parameters, recursive_self_ident));
                        return quote! { Self::parser(#(#arg_tokens),*).clone() };
                    }
                }

                let name_ident = idents
                    .get(name)
                    .unwrap_or_else(|| panic!("unknown rule {name}"));
                let arg_tokens = args
                    .iter()
                    .map(|a| Self::parser(a, idents, parameters, recursive_self_ident));
                quote! { #name_ident::parser(#(#arg_tokens),*) }
            }
        }
    }
    pub fn collect_deps(&self, deps: &mut std::collections::HashSet<String>) {
        match self {
            DslExpr::Just(name) => {
                deps.insert(name.clone());
            }
            DslExpr::Seq(exprs) => {
                for e in exprs {
                    e.collect_deps(deps);
                }
            }
            DslExpr::Opt(inner) | DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                inner.collect_deps(deps);
            }
            DslExpr::Alt(a, b) => {
                a.collect_deps(deps);
                b.collect_deps(deps);
            }
            DslExpr::Call { name, args } => {
                deps.insert(name.clone());
                for a in args {
                    a.collect_deps(deps);
                }
            }
        }
    }
}

pub fn dsl_parser<'a>() -> impl Parser<'a, &'a [DslToken], DslExpr> {
    recursive(|expr| {
        let ident = select! { DslToken::Ident(s) => s };

        let call = ident
            .clone()
            .then_ignore(just(DslToken::Lt))
            .then(
                expr.clone()
                    .separated_by(just(DslToken::Comma))
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(DslToken::Gt))
            .map(|(name, args)| DslExpr::Call { name, args });

        let atom = call
            .or(ident.map(DslExpr::Just))
            .or(expr
                .clone()
                .delimited_by(just(DslToken::LParen), just(DslToken::RParen)))
            .boxed();

        let unary = atom
            .clone()
            .then(
                choice((
                    just(DslToken::Question).map(|_| UnOp::Opt),
                    just(DslToken::Star).map(|_| UnOp::Star),
                    just(DslToken::Plus).map(|_| UnOp::Plus),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|(node, ops)| {
                ops.into_iter().fold(node, |acc, op| match op {
                    UnOp::Opt => DslExpr::Opt(Box::new(acc)),
                    UnOp::Star => DslExpr::Star(Box::new(acc)),
                    UnOp::Plus => DslExpr::Plus(Box::new(acc)),
                })
            });

        let seq = unary
            .clone()
            .then(unary.clone().repeated().collect::<Vec<_>>())
            .map(|(first, rest)| {
                if rest.is_empty() {
                    first
                } else {
                    let mut v = Vec::with_capacity(1 + rest.len());
                    v.push(first);
                    v.extend(rest);
                    DslExpr::Seq(v)
                }
            })
            .boxed();

        seq.clone()
            .then(
                just(DslToken::Pipe)
                    .ignore_then(seq.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(first, mut rest)| {
                if rest.is_empty() {
                    first
                } else {
                    let mut nested = rest.pop().unwrap();
                    for elt in rest.into_iter().rev() {
                        nested = DslExpr::Alt(Box::new(elt), Box::new(nested));
                    }
                    DslExpr::Alt(Box::new(first), Box::new(nested))
                }
            })
            .boxed()
    })
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    fn id<'a>(s: &'a str) -> DslToken {
        DslToken::Ident(s.to_owned())
    }

    #[test]
    fn test_just() {
        let tokens = vec![id("foo")];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Just("foo".to_owned()));
    }

    #[test]
    fn test_opt() {
        let tokens = vec![id("foo"), DslToken::Question];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Opt(Box::new(DslExpr::Just("foo".to_owned())))
        );
    }

    #[test]
    fn test_star() {
        let tokens = vec![id("bar"), DslToken::Star];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Star(Box::new(DslExpr::Just("bar".to_owned())))
        );
    }

    #[test]
    fn test_plus() {
        let tokens = vec![id("baz"), DslToken::Plus];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Plus(Box::new(DslExpr::Just("baz".to_owned())))
        );
    }

    #[test]
    fn test_seq() {
        let tokens = vec![id("a"), id("b"), id("c")];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Seq(vec![
                DslExpr::Just("a".into()),
                DslExpr::Just("b".into()),
                DslExpr::Just("c".into())
            ])
        );
    }

    #[test]
    fn test_alt() {
        let tokens = vec![id("x"), DslToken::Pipe, id("y"), DslToken::Pipe, id("z")];
        let expr = dsl_parser().parse(&tokens).unwrap();
        let expected = DslExpr::Alt(
            Box::new(DslExpr::Just("x".into())),
            Box::new(DslExpr::Alt(
                Box::new(DslExpr::Just("y".into())),
                Box::new(DslExpr::Just("z".into())),
            )),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_grouping() {
        let tokens = vec![
            DslToken::LParen,
            id("n"),
            DslToken::Plus,
            DslToken::RParen,
            DslToken::Star,
        ];
        let expr = dsl_parser().parse(&tokens).unwrap();
        let inner = DslExpr::Plus(Box::new(DslExpr::Just("n".into())));
        assert_eq!(expr, DslExpr::Star(Box::new(inner)));
    }
}

pub fn dsl_lexer<'a>() -> impl Parser<'a, &'a str, Vec<DslToken>, Err<Simple<'a, char>>> {
    let token = choice((
        text::ident().map(|text: &'a str| DslToken::Ident(text.to_owned())),
        just('?').to(DslToken::Question),
        just('*').to(DslToken::Star),
        just('+').to(DslToken::Plus),
        just('|').to(DslToken::Pipe),
        just('(').to(DslToken::LParen),
        just(')').to(DslToken::RParen),
        just('<').to(DslToken::Lt),
        just('>').to(DslToken::Gt),
        just(',').to(DslToken::Comma),
    ))
    .padded();

    token.repeated().collect().then_ignore(end())
}
#[cfg(test)]
mod lexer_tests {
    use super::*;
    use chumsky::Parser;

    #[test]
    fn test_lex_ident() {
        let tokens = dsl_lexer().parse("foo").unwrap();
        assert_eq!(tokens, vec![DslToken::Ident("foo".to_owned())]);
    }

    #[test]
    fn test_lex_symbols() {
        let tokens = dsl_lexer().parse("? * + | ( )").unwrap();
        assert_eq!(
            tokens,
            vec![
                DslToken::Question,
                DslToken::Star,
                DslToken::Plus,
                DslToken::Pipe,
                DslToken::LParen,
                DslToken::RParen
            ]
        );
    }

    #[test]
    fn test_parse_just() {
        let tokens = dsl_lexer().parse("bar").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Just("bar".to_owned()));
    }

    #[test]
    fn test_parse_opt() {
        let tokens = dsl_lexer().parse("baz?").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Opt(Box::new(DslExpr::Just("baz".to_owned())))
        );
    }

    #[test]
    fn test_parse_seq() {
        let tokens = dsl_lexer().parse("a b c").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Seq(vec![
                DslExpr::Just("a".to_owned()),
                DslExpr::Just("b".to_owned()),
                DslExpr::Just("c".to_owned())
            ])
        );
    }

    #[test]
    fn test_parse_alt() {
        let tokens = dsl_lexer().parse("x|y|z").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        let expected = DslExpr::Alt(
            Box::new(DslExpr::Just("x".to_owned())),
            Box::new(DslExpr::Alt(
                Box::new(DslExpr::Just("y".to_owned())),
                Box::new(DslExpr::Just("z".to_owned())),
            )),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_group_star() {
        let tokens = dsl_lexer().parse("(n)+").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Plus(Box::new(DslExpr::Just("n".to_owned()))));
    }
}
