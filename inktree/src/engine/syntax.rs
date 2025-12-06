use crate::{chumsky_ext::BuilderParser, engine::Parseable};

pub trait Syntax: cstree::Syntax + 'static {
    type Root: Parseable<Syntax = Self>;
    fn static_text(self) -> Option<&'static str>;

    fn from_raw(raw: cstree::RawSyntaxKind) -> Self;

    fn into_raw(self) -> cstree::RawSyntaxKind;

    fn parser<'src, 'cache, 'interner, 'borrow,'extra, Err>(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, Self>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'src: 'extra,
        'cache: 'extra;
        

    fn is_ast_relevant(&self) -> bool;
}
