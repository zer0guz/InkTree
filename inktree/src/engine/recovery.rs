// enum RecoverDsl {
//     SkipPast { term: Ident },
//     SkipTo { stops: Vec<Ident> },
//     List { sep: Ident, end: Ident },
//     Balanced { open: Ident, close: Ident },
//     // ...
// }

use chumsky::{
    error::EmptyErr,
    prelude::any,
    primitive::Any,
    recovery::{Strategy, ViaParser},
};

use crate::{Parseable, Syntax, chumsky_ext::BuilderParser};

pub struct Strict;
pub struct Recovering;

pub trait RecoverySpec<N: Parseable> {
    fn attach<'src, 'cache, 'interner, 'borrow, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'src;
}

pub struct NoRecovery;

impl<N: Parseable> RecoverySpec<N> for NoRecovery {
    fn attach<'src, 'cache, 'interner, 'borrow, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str>+ 'src,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'src
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
    fn attach<'src, 'cache, 'interner, 'borrow, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'src
    {
        use chumsky::Parser;
        use chumsky::recovery;

        let skip = any().ignored();

        let term = TermTok::parser::<'src, 'cache, 'interner, 'borrow, Err>().ignored();

        let strat = recovery::skip_until(skip, term, || {
            //TODO
            ()
        });

        parser.recover_with(strat)
    }
}

pub trait ParseMode<N: Parseable> {
    fn apply<'src, 'cache, 'interner, 'borrow, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'src;
}

impl<N: Parseable> ParseMode<N> for Strict {
    fn apply<'src, 'cache, 'interner, 'borrow, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone,
        'interner: 'cache,
        'borrow: 'interner
    {
        parser
    }
}

impl<N: Parseable> ParseMode<N> for Recovering {
    fn apply<'src, 'cache, 'interner, 'borrow, Err, P>(
        parser: P,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> +'src,
        P: BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, N::Syntax> + Clone,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'src
    {
        <N::RecoverySpec as RecoverySpec<N>>::attach::<
            'src,
            'cache,
            'interner,
            'borrow,
            Err,
            P,
        >(parser)
    }
}
