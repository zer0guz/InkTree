use std::collections::HashSet;

use chumsky::extra::Err;
use syn::Ident;

use crate::chumsky::prelude::*;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Just(String),
    Seq(Vec<Expr>),
    Opt(Box<Expr>),
    Star(Box<Expr>),
    Plus(Box<Expr>),
    Alt(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Question,
    Star,
    Plus,
    Pipe,
    LParen,
    RParen,
}

#[derive(Debug, Clone, Copy)]
enum UnOp {
    Opt,
    Star,
    Plus,
}

pub fn ebnf_parser<'a>() -> impl Parser<'a, &'a [Token], Expr> {
    recursive(|expr| {
        let atom = select! { Token::Ident(s) => Expr::Just(s.to_owned()) }.or(expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen)));

        let unary = atom
            .clone()
            .then(
                choice((
                    just(Token::Question).map(|_| UnOp::Opt),
                    just(Token::Star).map(|_| UnOp::Star),
                    just(Token::Plus).map(|_| UnOp::Plus),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|(node, ops)| {
                ops.into_iter().fold(node, |acc, op| match op {
                    UnOp::Opt => Expr::Opt(Box::new(acc)),
                    UnOp::Star => Expr::Star(Box::new(acc)),
                    UnOp::Plus => Expr::Plus(Box::new(acc)),
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
                    Expr::Seq(v)
                }
            });

        seq.clone()
            .then(
                just(Token::Pipe)
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
                        nested = Expr::Alt(Box::new(elt), Box::new(nested));
                    }
                    Expr::Alt(Box::new(first), Box::new(nested))
                }
            })
    })
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    fn id<'a>(s: &'a str) -> Token {
        Token::Ident(s.to_owned())
    }

    #[test]
    fn test_just() {
        let tokens = vec![id("foo")];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Just("foo".to_owned()));
    }

    #[test]
    fn test_opt() {
        let tokens = vec![id("foo"), Token::Question];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Opt(Box::new(Expr::Just("foo".to_owned()))));
    }

    #[test]
    fn test_star() {
        let tokens = vec![id("bar"), Token::Star];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Star(Box::new(Expr::Just("bar".to_owned()))));
    }

    #[test]
    fn test_plus() {
        let tokens = vec![id("baz"), Token::Plus];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Plus(Box::new(Expr::Just("baz".to_owned()))));
    }

    #[test]
    fn test_seq() {
        let tokens = vec![id("a"), id("b"), id("c")];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            Expr::Seq(vec![
                Expr::Just("a".into()),
                Expr::Just("b".into()),
                Expr::Just("c".into())
            ])
        );
    }

    #[test]
    fn test_alt() {
        let tokens = vec![id("x"), Token::Pipe, id("y"), Token::Pipe, id("z")];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        let expected = Expr::Alt(
            Box::new(Expr::Just("x".into())),
            Box::new(Expr::Alt(
                Box::new(Expr::Just("y".into())),
                Box::new(Expr::Just("z".into())),
            )),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_grouping() {
        let tokens = vec![
            Token::LParen,
            id("n"),
            Token::Plus,
            Token::RParen,
            Token::Star,
        ];
        let expr = ebnf_parser().parse(&tokens).unwrap();
        let inner = Expr::Plus(Box::new(Expr::Just("n".into())));
        assert_eq!(expr, Expr::Star(Box::new(inner)));
    }
}

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>, Err<Simple<'a, char>>> {
    let token = choice((
        text::ident().map(|text: &'a str| Token::Ident(text.to_owned())),
        just('?').to(Token::Question),
        just('*').to(Token::Star),
        just('+').to(Token::Plus),
        just('|').to(Token::Pipe),
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
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
        let tokens = lexer().parse("foo").unwrap();
        assert_eq!(tokens, vec![Token::Ident("foo".to_owned())]);
    }

    #[test]
    fn test_lex_symbols() {
        let tokens = lexer().parse("? * + | ( )").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Question,
                Token::Star,
                Token::Plus,
                Token::Pipe,
                Token::LParen,
                Token::RParen
            ]
        );
    }

    #[test]
    fn test_parse_just() {
        let tokens = lexer().parse("bar").unwrap();
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Just("bar".to_owned()));
    }

    #[test]
    fn test_parse_opt() {
        let tokens = lexer().parse("baz?").unwrap();
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Opt(Box::new(Expr::Just("baz".to_owned()))));
    }

    #[test]
    fn test_parse_seq() {
        let tokens = lexer().parse("a b c").unwrap();
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(
            expr,
            Expr::Seq(vec![
                Expr::Just("a".to_owned()),
                Expr::Just("b".to_owned()),
                Expr::Just("c".to_owned())
            ])
        );
    }

    #[test]
    fn test_parse_alt() {
        let tokens = lexer().parse("x|y|z").unwrap();
        let expr = ebnf_parser().parse(&tokens).unwrap();
        let expected = Expr::Alt(
            Box::new(Expr::Just("x".to_owned())),
            Box::new(Expr::Alt(
                Box::new(Expr::Just("y".to_owned())),
                Box::new(Expr::Just("z".to_owned())),
            )),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_group_star() {
        let tokens = lexer().parse("(n)+").unwrap();
        let expr = ebnf_parser().parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Plus(Box::new(Expr::Just("n".to_owned()))));
    }
}

