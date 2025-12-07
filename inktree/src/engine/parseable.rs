use crate::{
    Syntax,
    chumsky_ext::BuilderParser,
    engine::recovery::{ParseMode, Recovering, RecoverySpec},
};

pub trait Parseable: Sized
where
    Self::Syntax: Syntax,
{
    type Syntax;

    /// Describes how this node should recover when recovery is enabled.
    /// For nodes without #[inktree(recover(...))], we set this to NoRecovery.
    type RecoverySpec: RecoverySpec<Self>;

    /// Pure grammar, no recovery attached.
    fn parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone + 'extra
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
        Self::go::<'_, '_, '_, '_, '_, _, Recovering>()
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
        let base = Self::parser();

        Mode::apply(base)
    }
}
