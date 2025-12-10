use chumsky::{Parser, error::Error, extra::ParserExtra, prelude::just};

use crate::{
    KindBehaviorByTag, KindBuilderByTag, KindMarker, KindSpec, LangAlias, Language, Parseable,
    Syntax,
    chumsky_ext::{BuilderParser, GreenExtra},
};

/// Tag used to specialize behaviour for token kinds.
///
/// The boolean parameter differentiates static tokens (with `static_text`) from
/// non-static tokens (regex/pattern-based).
pub struct TagToken<const IS_STATIC: bool>;

/// Specification for a token kind at a given index.
///
/// This describes how to obtain the parser for the token. The `IS_STATIC`
/// parameter allows static and non-static tokens to be handled differently.
pub trait TokenSpec<const IS_STATIC: bool>: KindSpec<TagToken<IS_STATIC>> {
    /// Concrete type that provides the parser for this token.
    type Parser: Parseable;

    /// Default entry point for the token's base parser.
    ///
    /// Implementations can override this for additional wrapping, but most
    /// kinds can rely on the default delegation to `Parser::parser`.
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <Self::Parser as Parseable>::parser::<Extra>()
    }
}

impl<const IDX: u32, L> KindSpec<TagToken<true>> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<true>,
{
    #[type_const]
    const IDX: u32 = IDX;
    type Lang = L;
    type Tag = TagToken<true>;
}

impl<const IDX: u32, L> KindSpec<TagToken<false>> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<false>,
{
    #[type_const]
    const IDX: u32 = IDX;
    type Lang = L;
    type Tag = TagToken<false>;
}

/// Behaviour for static tokens, using the associated `TokenSpec` and `Parseable`.
impl<const IDX: u32, L> KindBehaviorByTag<TagToken<true>, L> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<true>,
{
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, L> as TokenSpec<true>>::Parser::parser::<Extra>()
    }
}

/// `Parseable` implementation for static token kinds.
///
/// This uses the language's `KINDS` array and `static_text` to construct a
/// `just("...")` parser for the corresponding token.
impl<const IDX: u32, L> Parseable for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<true>,
{
    fn parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        type Lang<const I: u32, const N: usize, T> = LangAlias<T, TagToken<true>>;
        let kind = Lang::<IDX, { L::COUNT }, Self>::KINDS[IDX as usize];
        let text = <Lang<IDX, { L::COUNT }, Self> as Syntax>::static_text(kind)
            .expect("static token kind must have static_text");
        just(text).ignored()
    }
}
impl<const IDX: u32, L> KindBuilderByTag<TagToken<true>, L> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<true>,
{
    fn builder_parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, L> + Clone + 'extra
    where
        Err: Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'extra,
        'src: 'extra,
    {
        let p = <KindMarker<IDX, L> as TokenSpec<true>>::base_parser::<
            'src,
            GreenExtra<'cache, 'interner, 'borrow, Err, L>,
        >();

        p.as_static_token(L::KINDS[IDX as usize])
    }
}

impl<const IDX: u32, L> KindBuilderByTag<TagToken<false>, L> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<false>,
{
    fn builder_parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, L> + Clone + 'extra
    where
        Err: Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'extra,
        'src: 'extra,
    {
        let p = <KindMarker<IDX, L> as TokenSpec<false>>::base_parser::<
            'src,
            GreenExtra<'cache, 'interner, 'borrow, Err, L>,
        >();

        p.as_token(L::KINDS[IDX as usize])
    }
}
