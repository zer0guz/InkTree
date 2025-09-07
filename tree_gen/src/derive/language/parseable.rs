
use crate::{
    chumsky_ext::BuilderParser,
    derive::language::Syntax,
    engine::Builder,
};

pub trait Parseable: Sized
where
    Self::Syntax: Syntax + 'static,
{
    type Syntax;

    fn parser<'src, 'cache, 'interner, 'borrow, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        Builder<'cache, 'interner, 'borrow, Self::Syntax>: chumsky::inspector::Inspector<'src, &'src str, Checkpoint = cstree::build::Checkpoint>
            + 'src,
        'interner: 'cache,
        'borrow: 'interner;
}
