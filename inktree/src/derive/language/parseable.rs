use crate::{chumsky_ext::BuilderParser, derive::language::Syntax};

pub trait Parseable: Sized
where
    Self::Syntax: Syntax + 'static,
{
    type Syntax;

    fn parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self::Syntax> + Clone
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra;
}
