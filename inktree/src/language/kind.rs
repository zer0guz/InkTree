use std::marker::PhantomData;

use chumsky::{extra::ParserExtra, prelude::just, Parser};

use crate::{Language, Parseable, Syntax};
use crate::language::node::{BuildableChildren, ChildList};

/// Shorthand to access the language associated with a kind specification.
type LangAlias<Spec, const IDX: u16, const N: usize, Tag> =
    <Spec as KindSpec<IDX, N, Tag>>::Lang;

/// Marker type for a concrete kind `IDX` of language `L` with `N` total kinds.
///
/// Each `KindMarker` acts as the central point where all behaviour for the
/// corresponding syntax kind is attached via traits.
pub struct KindMarker<const IDX: u16, const N: usize, L>(pub PhantomData<L>);

/// Tag used to specialize behaviour for token kinds.
///
/// The boolean parameter differentiates static tokens (with `static_text`) from
/// non-static tokens (regex/pattern-based).
pub struct TagToken<const IS_STATIC: bool>;

/// Tag used to specialize behaviour for node kinds.
pub struct TagNode;

/// Common information for all kinds in a language at a given index.
///
/// Implemented automatically for `KindMarker` when the corresponding
/// per-kind spec traits are implemented.
pub trait KindSpec<const IDX: u16, const N: usize, Tag> {
    /// Language that this kind belongs to.
    type Lang: Language<N>;

    /// Tag identifying the role of this kind (token, node, etc.).
    type Tag;
}

impl<const IDX: u16, const N: usize, L> KindSpec<IDX, N, TagToken<true>>
    for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, true>,
{
    type Lang = L;
    type Tag = TagToken<true>;
}

impl<const IDX: u16, const N: usize, L> KindSpec<IDX, N, TagToken<false>>
    for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, false>,
{
    type Lang = L;
    type Tag = TagToken<false>;
}

impl<const IDX: u16, const N: usize, L> KindSpec<IDX, N, TagNode> for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: NodeKindSpec<IDX, N>,
{
    type Lang = L;
    type Tag = TagNode;
}

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

/// Specification for a node kind at a given index.
///
/// The node's structure is described by an HList of children.
pub trait NodeKindSpec<const IDX: u16, const N: usize>: KindSpec<IDX, N, TagNode> {
    /// Type-level list of children for this node.
    type Children: ChildList<N, Self::Lang> + BuildableChildren<N, Self::Lang>;
}

/// Behaviour for a kind under a specific tag.
///
/// This is the type-level entry point to obtain a parser based on the kind's
/// role (token, node, etc.).
pub trait KindBehaviorByTag<L: Language<N>, const IDX: u16, const N: usize, Tag> {
    /// Build the base parser for this kind under the given tag.
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// Behaviour for static tokens, using the associated `TokenSpec` and `Parseable`.
impl<const IDX: u16, const N: usize, L> KindBehaviorByTag<
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

/// Behaviour for struct-like node kinds, built from their type-level child list.
impl<const IDX: u16, const N: usize, L> KindBehaviorByTag<
        <KindMarker<IDX, N, L> as KindSpec<IDX, N, TagNode>>::Lang,
        IDX,
        N,
        TagNode,
    > for KindMarker<IDX, N, L>
where
    L: Language<N>,
    KindMarker<IDX, N, L>: NodeKindSpec<IDX, N>,
{
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, N, L> as NodeKindSpec<IDX, N>>::Children::build_seq::<'src, Extra>()
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
