use crate::chumksy_ext::{BuilderParser, Input};

pub trait Syntax: cstree::Syntax + 'static {
    fn static_text(self) -> Option<&'static str>;

    fn from_raw(raw: cstree::RawSyntaxKind) -> Self;

    fn into_raw(self) -> cstree::RawSyntaxKind;

    fn parser<'src, 'cache, 'interner, Err>(
        self,
    ) -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Self>
    where
        Err: chumsky::error::Error<'src, Input<'src>> + 'src,
        Err: chumsky::label::LabelError<
                'src,
                &'src str,
                chumsky::text::TextExpected<'src, &'src str>,
            >,
        Err: chumsky::label::LabelError<'src, &'src str, &'src str>,
        'interner: 'cache,
        'cache: 'src;
}
