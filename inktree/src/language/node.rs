use std::marker::PhantomData;

use chumsky::{extra::ParserExtra, Parser};

use crate::{Language, Parseable};
use crate::language::kind::{KindMarker, TokenSpec};

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
pub struct Child<const IDX: u16, Card, Role>(pub PhantomData<(Card, Role)>);

/// Specification of a single child slot in a struct node.
pub trait ChildSpec<const N: usize, L: Language<N>> {
    /// Index of the child kind in the language's `KINDS` table.
    const IDX: u16;

    /// Cardinality for this child.
    type Card;

    /// Role for this child (token, node, error, …).
    type Role;
}

impl<const N: usize, L, const IDX: u16, Card, Role> ChildSpec<N, L> for Child<IDX, Card, Role>
where
    L: Language<N>,
{
    const IDX: u16 = IDX;
    type Card = Card;
    type Role = Role;
}

/// Type-level heterogeneous list of children for a struct node.
pub trait ChildList<const N: usize, L: Language<N>> {}

/// Empty child list.
pub struct Nil<const N: usize, L: Language<N>>(pub PhantomData<L>);

/// Cons cell for building type-level child lists.
pub struct Cons<Head, Tail>(pub PhantomData<(Head, Tail)>);

impl<const N: usize, L: Language<N>> ChildList<N, L> for Nil<N, L> {}

impl<const N: usize, L: Language<N>, Head, Tail> ChildList<N, L> for Cons<Head, Tail>
where
    Head: ChildSpec<N, L>,
    Tail: ChildList<N, L>,
{
}

/// Child list that can be turned into a sequential parser.
///
/// This is the high-level entry point for building struct-node parsers from
/// their type-level shape.
pub trait BuildableChildren<const N: usize, L: Language<N>>: ChildList<N, L> {
    /// Build a parser that consumes this child sequence.
    fn build_seq<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// `BuildableChildren` implementation for the empty child list.
impl<const N: usize, L> BuildableChildren<N, L> for Nil<N, L>
where
    L: Language<N>,
{
    fn build_seq<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <() as BuildStructSeq<N, L, Nil<N, L>>>::build::<'src, Extra>()
    }
}

/// `BuildableChildren` implementation for a leading `CardOne` child.
///
/// Additional `BuildableChildren` impls can be added for other cardinalities
/// if needed.
impl<const IDX: u16, const N: usize, Role, Tail, L> BuildableChildren<N, L>
    for Cons<Child<IDX, CardOne, Role>, Tail>
where
    L: Language<N>,
    Tail: ChildList<N, L> + BuildableChildren<N, L>,
    (): BuildStructSeq<N, L, Cons<Child<IDX, CardOne, Role>, Tail>>,
{
    fn build_seq<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <() as BuildStructSeq<N, L, Self>>::build::<'src, Extra>()
    }
}

/// Internal helper that folds a `ChildList` into a Chumsky sequence parser.
pub trait BuildStructSeq<const N: usize, L, List>
where
    L: Language<N>,
    List: ChildList<N, L>,
{
    /// Build the parser for the given child list.
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// `BuildStructSeq` implementation for an empty child list.
impl<const N: usize, L> BuildStructSeq<N, L, Nil<N, L>> for ()
where
    L: Language<N>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        chumsky::primitive::empty()
    }
}

/// Role-specific parser for a single child at a given index.
pub trait BaseChild<const IDX: u16, const N: usize, L: Language<N>, Role> {
    /// Build the parser for this child.
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// Token child implementation for static tokens.
///
/// This delegates to the `TokenSpec` and `Parseable` implementations for the
/// corresponding kind marker.
impl<const N: usize, L, const IDX: u16> BaseChild<IDX, N, L, RoleToken> for ()
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, true>,
{
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, N, L> as TokenSpec<IDX, N, true>>::Parser::parser::<Extra>().ignored()
    }
}

/// Token child implementation for non-static tokens.
impl<const IDX: u16, const N: usize, L> BaseChild<IDX, N, L, RoleToken> for ()
where
    L: Language<N>,
    KindMarker<IDX, N, L>: TokenSpec<IDX, N, false>,
{
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        <KindMarker<IDX, N, L> as TokenSpec<IDX, N, false>>::Parser::parser::<Extra>().ignored()
    }
}

/// Node child implementation.
///
/// This will later dispatch to the node parser; currently it is a placeholder.
impl<const IDX: u16, const N: usize, L> BaseChild<IDX, N, L, RoleNode> for ()
where
    L: Language<N>,
{
    fn base<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        chumsky::primitive::empty()
    }
}

/// `BuildStructSeq` implementation for a leading `CardOne` child.
impl<const IDX: u16, const N: usize, L, Role, Tail>
    BuildStructSeq<N, L, Cons<Child<IDX, CardOne, Role>, Tail>> for ()
where
    L: Language<N>,
    Tail: ChildList<N, L>,
    (): BuildStructSeq<N, L, Tail>,
    (): BaseChild<IDX, N, L, Role>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let head = <() as BaseChild<IDX, N, L, Role>>::base::<'src, Extra>();
        let tail = <() as BuildStructSeq<N, L, Tail>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// `BuildStructSeq` implementation for a leading `CardZeroOrOne` child.
impl<const IDX: u16, const N: usize, L, Role, Tail>
    BuildStructSeq<N, L, Cons<Child<IDX, CardZeroOrOne, Role>, Tail>> for ()
where
    L: Language<N>,
    Tail: ChildList<N, L>,
    (): BuildStructSeq<N, L, Tail>,
    (): BaseChild<IDX, N, L, Role>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let base = <() as BaseChild<IDX, N, L, Role>>::base::<'src, Extra>();
        let head = base.or_not().ignored();
        let tail = <() as BuildStructSeq<N, L, Tail>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// `BuildStructSeq` implementation for a leading `CardMany` child.
impl<const IDX: u16, const N: usize, L, Role, Tail>
    BuildStructSeq<N, L, Cons<Child<IDX, CardMany, Role>, Tail>> for ()
where
    L: Language<N>,
    Tail: ChildList<N, L>,
    (): BuildStructSeq<N, L, Tail>,
    (): BaseChild<IDX, N, L, Role>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let base = <() as BaseChild<IDX, N, L, Role>>::base::<'src, Extra>();
        let head = base.repeated().ignored();
        let tail = <() as BuildStructSeq<N, L, Tail>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}

/// `BuildStructSeq` implementation for a leading `CardOneOrMore` child.
impl<const IDX: u16, const N: usize, L, Role, Tail>
    BuildStructSeq<N, L, Cons<Child<IDX, CardOneOrMore, Role>, Tail>> for ()
where
    L: Language<N>,
    Tail: ChildList<N, L>,
    (): BuildStructSeq<N, L, Tail>,
    (): BaseChild<IDX, N, L, Role>,
{
    fn build<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        let base = <() as BaseChild<IDX, N, L, Role>>::base::<'src, Extra>();
        let head = base.repeated().at_least(1).ignored();
        let tail = <() as BuildStructSeq<N, L, Tail>>::build::<'src, Extra>();
        head.then(tail).ignored()
    }
}
