use crate::{chumsky_ext::BuilderParser, derive::language::Syntax};

pub trait Parseable: Sized
where
    Self::Syntax: Syntax + 'static,
{
    type Syntax;

    fn parser<'src, 'cache, 'interner, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Self::Syntax>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        'cache: 'src,
        'interner: 'cache;
}
