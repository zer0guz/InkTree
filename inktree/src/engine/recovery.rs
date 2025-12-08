use chumsky::{Parser, prelude::any};

use crate::{
    Parseable, Syntax,
    chumsky_ext::{BuilderParser, GreenExtra},
};

pub struct Strict;
pub struct Recovering;

pub trait RecoverySpec<N: Parseable> {
    fn attach<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra;
}

pub struct NoRecovery;

impl<N: Parseable> RecoverySpec<N> for NoRecovery {
    fn attach<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str>,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra,
    {
        parser
    }
}

pub struct SkipPast<TermTok>(std::marker::PhantomData<TermTok>);

impl<TermTok> Clone for SkipPast<TermTok> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<N, TermTok: 'static> RecoverySpec<N> for SkipPast<TermTok>
where
    N: Parseable + 'static,
    N::Syntax: Syntax,
    TermTok: Parseable<Syntax = N::Syntax>,
{
    fn attach<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra,
    {
        any::<&str, GreenExtra<Err, N::Syntax>>()
            .and_is(TermTok::parser().not())
            .ignored()
            .repeated()
            .at_least(1)
            .to_slice()
            .as_token(<N::Syntax as Syntax>::ERROR)
    }
}

pub trait ParseMode<N: Parseable> {
    const NAME: &'static str;
    fn apply<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra;
}

impl<N: Parseable> ParseMode<N> for Strict {
    const NAME: &'static str = "Strict";

    fn apply<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
    {
        eprint!("strict mode\n");
        parser
    }
}

impl<N: Parseable> ParseMode<N> for Recovering {
    const NAME: &'static str = "Recovering\n";

    fn apply<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra,
    {
        eprint!("recovery mode");
        <N::RecoverySpec as RecoverySpec<N>>::attach(parser)
    }
}
