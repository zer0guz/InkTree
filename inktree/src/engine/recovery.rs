use chumsky::{Parser, prelude::any};

use crate::{
    Parseable, Syntax,
    chumsky_ext::{BuilderParser, recover},
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

impl<N, TermTok> RecoverySpec<N> for SkipPast<TermTok>
where
    N: Parseable,
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
        let rec = any().repeated().at_least(1).to_slice();

        recover(parser, rec)
    }
}

pub trait ParseMode<N: Parseable> {
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
    fn apply<'src, 'cache, 'interner, 'borrow, 'extra, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
    {
        parser
    }
}

impl<N: Parseable> ParseMode<N> for Recovering {
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
        <N::RecoverySpec as RecoverySpec<N>>::attach(parser)
    }
}
