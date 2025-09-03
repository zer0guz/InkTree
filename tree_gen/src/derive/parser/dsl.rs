use chumsky::extra::Err;
use std::collections::{HashMap, HashSet};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use crate::{chumsky::prelude::*, derive::attributes::Rule};

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum DslExpr {
    Just(Ident),
    Seq(Vec<DslExpr>),
    Opt(Box<DslExpr>),
    Star(Box<DslExpr>),
    Plus(Box<DslExpr>),
    Alt(Box<DslExpr>, Box<DslExpr>),
    Call { name: Ident, args: Vec<DslExpr> },
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

#[derive(Hash, Eq, PartialEq)]
pub struct CallShape<'a> {
    callee: Ident,
    args: &'a [DslExpr], // serialized expr...
}

impl<'a> CallShape<'a> {
    fn new(callee: Ident, args: &'a [DslExpr]) -> Self {
        CallShape::<'a> { callee, args: args }
    }
}

pub struct ParserCtx<'a> {
    pub idents: &'a HashSet<Ident>,
    pub parameters: &'a HashSet<Ident>,
    pub anchored: HashSet<Ident>,
}

impl<'a> ParserCtx<'a> {
    pub fn new(
        idents: &'a HashSet<Ident>,
        parameters: &'a HashSet<Ident>,
        anchored: HashSet<Ident>,
    ) -> Self {
        Self {
            idents,
            parameters,
            anchored,
        }
    }
}

impl DslExpr {
    pub fn parser(&self, ctx: &ParserCtx) -> TokenStream {
        match self {
            DslExpr::Just(name) => Self::call_parser(name, &[], ctx),

            DslExpr::Seq(exprs) => {
                let folded = exprs
                    .iter()
                    .map(|e| e.parser(ctx))
                    .fold(quote! {}, |acc, p| {
                        if acc.is_empty() {
                            p
                        } else {
                            quote! { #acc.then(#p) }
                        }
                    });
                quote! { #folded.ignored() }
            }

            DslExpr::Opt(inner) => {
                let p = inner.parser(ctx);
                quote! { #p.or_not() }
            }

            DslExpr::Star(inner) => {
                let p = inner.parser(ctx);
                quote! { #p.repeated() }
            }

            DslExpr::Plus(inner) => {
                let p = inner.parser(ctx);
                quote! { #p.repeated().at_least(1) }
            }

            DslExpr::Alt(a, b) => {
                let pa = a.parser(ctx);
                let pb = b.parser(ctx);
                quote! { #pa.or(#pb) }
            }

            DslExpr::Call { name, args } => {
                let arg_tokens = args.iter().map(|a| a.parser(ctx)).collect::<Vec<_>>();
                Self::call_parser(name, &arg_tokens, ctx)
            }
        }
    }

    fn call_parser(name: &Ident, arg_tokens: &[TokenStream], ctx: &ParserCtx) -> TokenStream {
        if ctx.anchored.contains(name) {
            let ident = Ident::new(&name.to_string().to_lowercase(), Span::call_site());
            return quote! { #ident };
        }

        // ---- not in same SCC ----
        if let Some(ident) = ctx.idents.get(name) {
            quote! { #ident::parser(#(#arg_tokens),*) }
        } else if let Some(ident) = ctx.parameters.get(name) {
            quote! { #ident.clone() }
        } else {
            panic!("unknown rule {name:?}")
        }
    }

    pub fn collect_deps(&self, parameters: &HashSet<Ident>, deps: &mut HashSet<Ident>) {
        match self {
            DslExpr::Just(name) => {
                // Only add if it's not a parameter of the current rule
                if !parameters.contains(name) {
                    deps.insert(name.clone());
                }
            }
            DslExpr::Seq(exprs) => {
                for e in exprs {
                    e.collect_deps(parameters, deps);
                }
            }
            DslExpr::Opt(inner) | DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                inner.collect_deps(parameters, deps);
            }
            DslExpr::Alt(a, b) => {
                a.collect_deps(parameters, deps);
                b.collect_deps(parameters, deps);
            }
            DslExpr::Call { name, args } => {
                // Always add the callee
                deps.insert(name.clone());
                // Args can themselves contain rule references
                for a in args {
                    a.collect_deps(parameters, deps);
                }
            }
        }
    }

    /// Precise nullability: does this expr derive ε?
    /// Every symbol is a rule, so `Just` and `Call` look up their nullability
    /// in the fixed-point map.
    pub fn nullable_with(&self, nullable: &HashMap<Ident, bool>) -> bool {
        match self {
            DslExpr::Just(name) | DslExpr::Call { name, .. } => {
                *nullable.get(name).unwrap_or(&false)
            }
            DslExpr::Seq(items) => items.iter().all(|e| e.nullable_with(nullable)),
            DslExpr::Opt(_) | DslExpr::Star(_) => true,
            DslExpr::Plus(inner) => inner.nullable_with(nullable),
            DslExpr::Alt(a, b) => a.nullable_with(nullable) || b.nullable_with(nullable),
        }
    }

    pub fn first_nonterminals_with<'a>(
        &'a self,
        origin: &Ident,
        nullable: &HashMap<Ident, bool>,
        rules: &HashMap<Ident, &'a Rule>,
        subst: &HashMap<Ident, &'a DslExpr>,
        out: &mut HashSet<Ident>,
        visiting: &mut HashSet<CallShape<'a>>,
    ) {
        match self {
            DslExpr::Just(name) => {
                if let Some(arg_expr) = subst.get(name) {
                    // Instead of treating as a leaf, recurse into the argument
                    arg_expr.first_nonterminals_with(origin, nullable, rules, subst, out, visiting);
                } else {
                    out.insert(name.clone());
                }
            }

            DslExpr::Call { name, args } => {
                let shape = CallShape::new(name.clone(), args.as_slice());

                if !visiting.insert(shape) {
                    // We detected A -> ... -> A via parameters
                    out.insert(name.clone()); // current callee
                    // And also the *origin*, which is the rule we're analyzing
                    // That origin is `a` in build_left_corner_graph
                    // So you need to pass it down to first_nonterminals_with
                    out.insert(origin.clone());
                    return;
                }

                if let Some(rule) = rules.get(name) {
                    // Build a fresh substitution for this call
                    let mut inner_subst = HashMap::new();
                    for (param, arg_expr) in rule.parameters.iter().zip(args) {
                        // Directly record the *expression* (not just Just idents)
                        inner_subst.insert(param.clone(), arg_expr);
                    }

                    // Recurse into callee body with substitution
                    rule.dsl.first_nonterminals_with(
                        origin,
                        nullable,
                        rules,
                        &inner_subst,
                        out,
                        visiting,
                    );
                }
            }

            DslExpr::Seq(items) => {
                for e in items {
                    e.first_nonterminals_with(origin, nullable, rules, subst, out, visiting);
                    if !e.nullable_with(nullable) {
                        break;
                    }
                }
            }

            DslExpr::Alt(a, b) => {
                a.first_nonterminals_with(origin, nullable, rules, subst, out, visiting);
                b.first_nonterminals_with(origin, nullable, rules, subst, out, visiting);
            }

            DslExpr::Opt(inner) | DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                inner.first_nonterminals_with(origin, nullable, rules, subst, out, visiting);
            }
        }
    }
}

pub fn dsl_parser<'a>() -> impl Parser<'a, &'a [DslToken], DslExpr> {
    recursive(|expr| {
        let ident = select! { DslToken::Ident(s) => Ident::new(&s, Span::call_site()) };

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
mod tests {
    use super::*;
    use proc_macro2::{Ident, Span};

    fn id_expr(s: &str) -> DslExpr {
        DslExpr::Just(Ident::new(s, Span::call_site()))
    }

    fn id_tok(s: &str) -> DslToken {
        DslToken::Ident(s.to_owned())
    }

    #[test]
    fn test_just() {
        let tokens = vec![id_tok("foo")];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, id_expr("foo"));
    }

    #[test]
    fn test_opt() {
        let tokens = vec![id_tok("foo"), DslToken::Question];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Opt(Box::new(id_expr("foo"))));
    }

    #[test]
    fn test_star() {
        let tokens = vec![id_tok("bar"), DslToken::Star];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Star(Box::new(id_expr("bar"))));
    }

    #[test]
    fn test_plus() {
        let tokens = vec![id_tok("baz"), DslToken::Plus];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Plus(Box::new(id_expr("baz"))));
    }

    #[test]
    fn test_seq() {
        let tokens = vec![id_tok("a"), id_tok("b"), id_tok("c")];
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Seq(vec![id_expr("a"), id_expr("b"), id_expr("c")])
        );
    }

    #[test]
    fn test_alt() {
        let tokens = vec![
            id_tok("x"),
            DslToken::Pipe,
            id_tok("y"),
            DslToken::Pipe,
            id_tok("z"),
        ];
        let expr = dsl_parser().parse(&tokens).unwrap();
        let expected = DslExpr::Alt(
            Box::new(id_expr("x")),
            Box::new(DslExpr::Alt(Box::new(id_expr("y")), Box::new(id_expr("z")))),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_grouping() {
        let tokens = vec![
            DslToken::LParen,
            id_tok("n"),
            DslToken::Plus,
            DslToken::RParen,
            DslToken::Star,
        ];
        let expr = dsl_parser().parse(&tokens).unwrap();
        let inner = DslExpr::Plus(Box::new(id_expr("n")));
        assert_eq!(expr, DslExpr::Star(Box::new(inner)));
    }

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
        assert_eq!(expr, id_expr("bar"));
    }

    #[test]
    fn test_parse_opt() {
        let tokens = dsl_lexer().parse("baz?").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Opt(Box::new(id_expr("baz"))));
    }

    #[test]
    fn test_parse_seq() {
        let tokens = dsl_lexer().parse("a b c").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            DslExpr::Seq(vec![id_expr("a"), id_expr("b"), id_expr("c")])
        );
    }

    #[test]
    fn test_parse_alt() {
        let tokens = dsl_lexer().parse("x|y|z").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        let expected = DslExpr::Alt(
            Box::new(id_expr("x")),
            Box::new(DslExpr::Alt(Box::new(id_expr("y")), Box::new(id_expr("z")))),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_group_star() {
        let tokens = dsl_lexer().parse("(n)+").unwrap();
        let expr = dsl_parser().parse(&tokens).unwrap();
        assert_eq!(expr, DslExpr::Plus(Box::new(id_expr("n"))));
    }
}
