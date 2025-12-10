use std::marker::PhantomData;

use chumsky::error::Error;
use chumsky::prelude::empty;
use chumsky::{Parser, extra::ParserExtra};

use crate::chumsky_ext::{BuilderParser, GreenExtra};
use crate::language::kind::KindMarker;
use crate::{KindBehaviorByTag, KindBuilderByTag, KindSpec, Language, Parseable, TokenSpec};

/// Tag used to specialize behaviour for node kinds.
pub struct TagNode;

impl<const IDX: u32, L> KindSpec<TagNode> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: NodeKindSpec<IDX>,
{
    type Lang = L;
    type Tag = TagNode;

    #[type_const]
    const IDX: u32 = IDX;
}

/// Specification for a node kind at a given index.
///
/// The node's structure is described by an HList of children.
pub trait NodeKindSpec<const IDX: u32>: KindSpec<TagNode> {
    /// Type-level list of children for this node.
    type Children: ChildList<Self::Lang> + BuildableChildren<Self::Lang>;
}

/// Behaviour for struct-like node kinds, built from their type-level child list.
impl<const IDX: u32, L> KindBehaviorByTag<TagNode, L> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: NodeKindSpec<IDX>,
{
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, L> as NodeKindSpec<IDX>>::Children::build_seq::<'src, Extra>()
    }
}

/// Marker type for a token child in a struct node definition.
pub struct RoleToken;

/// Marker type for a node child in a struct node definition.
pub struct RoleNode;

/// Marker type reserved for error or recovery children.
pub struct RoleError;

/// Cardinality marker for a required child (`1`).
pub struct CardOne;

/// Cardinality marker for an optional child (`?`).
pub struct CardZeroOrOne;

/// Cardinality marker for zero or more children (`*`).
pub struct CardMany;

/// Cardinality marker for one or more children (`+`).
pub struct CardOneOrMore;

/// Pointer to a child kind with role and cardinality information.
pub struct Child<const IDX: u32, Card, Role>(pub PhantomData<(Card, Role)>);

/// Specification of a single child slot in a struct node.
pub trait ChildSpec<L: Language> {
    /// Index of the child kind in the language's `KINDS` table.
    const IDX: u32;

    /// Cardinality for this child.
    type Card;

    /// Role for this child (token, node, error, …).
    type Role;
}

impl<const IDX: u32, Card, Role, L> ChildSpec<L> for Child<IDX, Card, Role>
where
    L: Language,
{
    const IDX: u32 = IDX;
    type Card = Card;
    type Role = Role;
}

/// Type-level heterogeneous list of children for a struct node.
pub trait ChildList<L: Language> {}

/// Empty child list.
pub struct Nil<L: Language>(pub PhantomData<L>);

/// Cons cell for building type-level child lists.
pub struct Cons<Head, Tail>(pub PhantomData<(Head, Tail)>);

impl<L: Language> ChildList<L> for Nil<L> {}

impl<L: Language, Head, Tail> ChildList<L> for Cons<Head, Tail>
where
    Head: ChildSpec<L>,
    Tail: ChildList<L>,
{
}

/// Child list that can be turned into a sequential parser.
///
/// This is the high-level entry point for building struct-node parsers from
/// their type-level shape.
pub trait BuildableChildren<L: Language>: ChildList<L> {
    /// Build a parser that consumes this child sequence.
    fn build_seq<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// `BuildableChildren` implementation for the empty child list.
impl<L> BuildableChildren<L> for Nil<L>
where
    L: Language,
{
    fn build_seq<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        empty()
    }
}

/// `BuildableChildren` implementation for a leading `CardOne` child.
///
/// Additional `BuildableChildren` impls can be added for other cardinalities
/// if needed.
impl<const IDX: u32, Role, Tail, L> BuildableChildren<L> for Cons<Child<IDX, CardOne, Role>, Tail>
where
    L: Language,
    Tail: ChildList<L> + BuildableChildren<L>,
    Const: BuildStructSeq<Cons<Child<IDX, CardOne, Role>, Tail>, L>,
{
    fn build_seq<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        Const::build::<'src, Extra>()
    }
}

/// Internal helper that folds a `ChildList` into a Chumsky sequence parser.
pub trait BuildStructSeq<List, L>
where
    L: Language,
    List: ChildList<L>,
{
    /// Build the parser for the given child list.
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// `BuildStructSeq` implementation for an empty child list.
impl<L> BuildStructSeq<Nil<L>, L> for Const
where
    L: Language,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        chumsky::primitive::empty()
    }
}

/// Role-specific parser for a single child at a given index.
pub trait BaseChild<const IDX: u32, Role, L: Language> {
    /// Build the parser for this child.
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

pub struct Const;

/// Token child implementation for static tokens.
///
/// This delegates to the `TokenSpec` and `Parseable` implementations for the
/// corresponding kind marker.
impl<L, const IDX: u32> BaseChild<IDX, RoleToken, L> for Const
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<true>,
{
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, L> as TokenSpec<true>>::Parser::parser::<Extra>().ignored()
    }
}

/// Token child implementation for non-static tokens.
impl<const IDX: u32, L> BaseChild<IDX, RoleToken, L> for Const
where
    L: Language,
    KindMarker<IDX, L>: TokenSpec<false>,
{
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, L> as TokenSpec<false>>::Parser::parser::<Extra>().ignored()
    }
}

/// Node child implementation.
///
/// This will later dispatch to the node parser; currently it is a placeholder.
impl<const IDX: u32, L> BaseChild<IDX, RoleNode, L> for Const
where
    L: Language,
{
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        chumsky::primitive::empty()
    }
}

/// `BuildStructSeq` implementation for a leading `CardOne` child.
impl<const IDX: u32, Role, Tail, L> BuildStructSeq<Cons<Child<IDX, CardOne, Role>, Tail>, L>
    for Const
where
    L: Language,
    Tail: ChildList<L>,
    Const: BuildStructSeq<Tail, L>,
    Const: BaseChild<IDX, Role, L>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let head = <Const as BaseChild<IDX, Role, L>>::base::<'src, Extra>();
        let tail = <Const as BuildStructSeq<Tail, L>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// `BuildStructSeq` implementation for a leading `CardZeroOrOne` child.
impl<const IDX: u32, L, Role, Tail> BuildStructSeq<Cons<Child<IDX, CardZeroOrOne, Role>, Tail>, L>
    for Const
where
    L: Language,
    Tail: ChildList<L>,
    Const: BuildStructSeq<Tail, L>,
    Const: BaseChild<IDX, Role, L>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let base = <Const as BaseChild<IDX, Role, L>>::base::<'src, Extra>();
        let head = base.or_not().ignored();
        let tail = <Const as BuildStructSeq<Tail, L>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// `BuildStructSeq` implementation for a leading `CardMany` child.
impl<const IDX: u32, Role, Tail, L> BuildStructSeq<Cons<Child<IDX, CardMany, Role>, Tail>, L>
    for Const
where
    L: Language,
    Tail: ChildList<L>,
    Const: BuildStructSeq<Tail, L>,
    Const: BaseChild<IDX, Role, L>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let base = <Const as BaseChild<IDX, Role, L>>::base::<'src, Extra>();
        let head = base.repeated().ignored();
        let tail = <Const as BuildStructSeq<Tail, L>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// `BuildStructSeq` implementation for a leading `CardOneOrMore` child.
impl<const IDX: u32, Role, Tail, L> BuildStructSeq<Cons<Child<IDX, CardOneOrMore, Role>, Tail>, L>
    for Const
where
    L: Language,
    Tail: ChildList<L>,
    Const: BuildStructSeq<Tail, L>,
    Const: BaseChild<IDX, Role, L>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let base = <Const as BaseChild<IDX, Role, L>>::base::<'src, Extra>();
        let head = base.repeated().at_least(1).ignored();
        let tail = <Const as BuildStructSeq<Tail, L>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// Builds a `chumsky` parser for a struct-like node described by a type-level
/// child list.
///
/// The `List` type is an HList composed of [`Cons`] and [`Nil`] with
/// [`Child<IDX, Card, Role>`] elements. Each child entry expands to the
/// appropriate `chumsky` parser based on its role and cardinality.
///
/// This example uses a stand-in language `MiniLang` with two token kinds `A`
/// and `B`, and a struct node that parses `A B`.
///
/// ```
/// use inktree::language::{struct_parser, Cons, Nil, Child, CardOne, RoleToken};
///
/// // Minimal example language enum:
/// #[repr(u16)]
/// #[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// enum MiniLang {
///     A,
///     B,
/// }
///
/// const N: usize = 2;
///
/// // Type-level child list: "A" then "B"
/// type Children = Cons<
///     Child<{ MiniLang::A as u16 }, CardOne, RoleToken>,
///     Cons<Child<{ MiniLang::B as u16 }, CardOne, RoleToken>, Nil<N, MiniLang>>,
/// >;
///
/// // Build a parser for that sequence. In real code you would plug in a real
/// // `Language<N>` and `Syntax` impl for `MiniLang`.
/// let _parser = struct_parser::<N, Children, MiniLang, chumsky::extra::Err<_, _>>();
/// ```

pub fn struct_parser<'src, List, L, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
where
    L: Language,
    List: ChildList<L>,
    Const: BuildStructSeq<List, L>,
    Extra: ParserExtra<'src, &'src str>,
{
    Const::build::<'src, Extra>()
}

impl<const IDX: u32, L> KindBuilderByTag<TagNode, L> for KindMarker<IDX, L>
where
    L: Language,
    KindMarker<IDX, L>: NodeKindSpec<IDX>,
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
        // Children HList → grammar parser, with GreenExtra
        let seq = <KindMarker<IDX, L> as NodeKindSpec<IDX>>::Children::build_seq::<
            'src,
            GreenExtra<'cache, 'interner, 'borrow, Err, L>,
        >();

        let kind = L::KINDS[IDX as usize];

        seq.as_node(kind)
    }
}
