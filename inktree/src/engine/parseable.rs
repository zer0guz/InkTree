use crate::{
    Syntax,
    chumsky_ext::BuilderParser,
    engine::recovery::{ParseMode, Recovering, RecoverySpec, Strict},
};

pub trait Parseable: Sized
where
    Self::Syntax: Syntax,
{
    type Syntax;
    type RecoverySpec: RecoverySpec<Self>;

    fn base_parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'extra,
        'src: 'extra;

    fn wrap<'src, 'cache, 'interner, 'borrow, 'extra, Err>(
        base: impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax>
        + Clone
        + 'extra,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'extra,
        'src: 'extra;

    fn recover<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra,
        Self: 'extra,
    {
        Self::go::<_, Recovering>()
    }
    fn parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra,
        Self: 'extra,
    {
        Self::go::<_, Strict>()
    }

    fn go<'src, 'cache, 'interner, 'borrow, 'extra, Err, Mode>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone + 'extra
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        Mode: ParseMode<Self> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        Self: 'extra,
        'src: 'extra,
        'cache: 'extra,
    {
        let base = Self::base_parser();
        eprint!("going with: {:#?}\n", Mode::NAME);
        Self::wrap(Mode::apply(base))
    }
}
