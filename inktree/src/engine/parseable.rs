use crate::{
    Syntax,
    chumsky_ext::BuilderParser,
    engine::recovery::{ParseMode, RecoverySpec},
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
    fn parser<'src, 'cache, 'interner, 'borrow, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        'cache: 'src,
        'interner: 'cache,
        'borrow: 'interner;

    fn go<'src, 'cache, 'interner, 'borrow, Err, Mode>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str>  + 'src,
        Mode: ParseMode<Self>,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'src
    {
        let base = Self::parser();

        Mode::apply(base)
    }
}
