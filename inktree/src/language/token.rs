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
pub trait TokenSpec<const IDX: u16, const N: usize, const IS_STATIC: bool>:
    KindSpec<IDX, N, TagToken<IS_STATIC>>
{
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

impl<const IDX: u16, const N: usize, L> KindSpec<IDX, N, TagToken<true>> for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, true>,
{
    type Lang = L;
    type Tag = TagToken<true>;
}

impl<const IDX: u16, const N: usize, L> KindSpec<IDX, N, TagToken<false>> for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, false>,
{
    type Lang = L;
    type Tag = TagToken<false>;
}

/// Behaviour for static tokens, using the associated `TokenSpec` and `Parseable`.
impl<const IDX: u16, const N: usize, L>
    KindBehaviorByTag<
        <KindMarker<IDX, N, L> as KindSpec<IDX, N, TagToken<true>>>::Lang,
        IDX,
        N,
        TagToken<true>,
    > for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, true>,
{
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, N, L> as TokenSpec<IDX, N, true>>::Parser::parser::<Extra>()
    }
}

/// `Parseable` implementation for static token kinds.
///
/// This uses the language's `KINDS` array and `static_text` to construct a
/// `just("...")` parser for the corresponding token.
impl<const IDX: u16, const N: usize, L> Parseable for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, true>,
{
    fn parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        type Lang<const I: u16, const N: usize, T> = LangAlias<T, I, N, TagToken<true>>;
        let kind = Lang::<IDX, N, Self>::KINDS[IDX as usize];
        let text = <Lang<IDX, N, Self> as Syntax>::static_text(kind)
            .expect("static token kind must have static_text");
        just(text).ignored()
    }
}
impl<const IDX: u16, const N: usize, L> KindBuilderByTag<L, IDX, N, TagToken<true>>
    for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, true>,
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
        let p = <KindMarker<IDX, N, L> as TokenSpec<IDX, N, true>>::base_parser::<
            'src,
            GreenExtra<'cache, 'interner, 'borrow, Err, L>,
        >();

        p.as_static_token(L::KINDS[IDX as usize])
    }
}

impl<const IDX: u16, const N: usize, L> KindBuilderByTag<L, IDX, N, TagToken<false>>
    for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, false>,
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
        let p = <KindMarker<IDX, N, L> as TokenSpec<IDX, N, false>>::base_parser::<
            'src,
            GreenExtra<'cache, 'interner, 'borrow, Err, L>,
        >();

        p.as_token(L::KINDS[IDX as usize])
    }
}
