use chumsky::inspector::Inspector;

use crate::{chumsky_ext::{BuilderParser, GreenState}, engine::Builder, Parseable};

pub trait Syntax: cstree::Syntax + 'static {
    type Root: Parseable<Syntax = Self>;
    fn static_text(self) -> Option<&'static str>;

    fn from_raw(raw: cstree::RawSyntaxKind) -> Self;

    fn into_raw(self) -> cstree::RawSyntaxKind;

    fn parser<'src, 'cache, 'interner,'borrow, Err>(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner,'borrow, (), Err, Self>
       where
        Builder<'cache, 'interner, 'borrow, Self>: Inspector<'src, &'src str, Checkpoint = cstree::build::Checkpoint> + GreenState<'src, Self> + 'src,
        Err: chumsky::error::Error<'src, &'src str> + 'src + 'src,
        'interner: 'cache,
        'borrow: 'interner;
}
