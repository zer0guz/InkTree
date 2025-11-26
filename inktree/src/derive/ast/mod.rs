mod macros;
mod shape;
mod state;

use cstree::interning::Resolver;
pub use shape::*;
pub use state::*;

use crate::Syntax;

#[derive(Clone, Copy, Debug)]

pub struct ParseError;

pub type SyntaxNode<Sy> = cstree::syntax::SyntaxNode<Sy, ParseError>;
pub type SyntaxToken<Sy> = cstree::syntax::SyntaxToken<Sy, ParseError>;

use core::{marker::PhantomData, ops::Deref};

#[derive(Debug, Clone)]
pub struct AstNodeWrapper<K, S, Sy: Syntax>(
    pub(crate) SyntaxNode<Sy>,
    pub(crate) PhantomData<(K, S)>,
);

#[derive(Debug, Clone)]
pub struct AstTokenWrapper<K, S, Sy: Syntax>(
    pub(crate) SyntaxToken<Sy>,
    pub(crate) PhantomData<(K, S)>,
);

impl<K, S, Sy: Syntax> AstTokenWrapper<K, S, Sy> {
    pub fn new(syntax_token: SyntaxToken<Sy>) -> Self {
        Self(syntax_token, PhantomData)
    }
}

impl<K, S, Sy: Syntax> Deref for AstNodeWrapper<K, S, Sy> {
    type Target = SyntaxNode<Sy>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<K, S, Sy: Syntax> Deref for AstTokenWrapper<K, S, Sy> {
    type Target = SyntaxToken<Sy>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Safety: `KINDS` must list exactly the syntax kinds this marker represents.
/// Mismatches can violate verified invariants in your accessors.
pub unsafe trait Kind {
    type Syntax: Syntax;
    const KINDS: &'static [Self::Syntax];
}

/// Safety: Implementors must guarantee that in a verified tree,
/// every `P` node always has a `K` child.
pub unsafe trait RequiredChild<P>: NodeKind {}

pub trait View: Sized {
    type Kind: NodeKind;
    type State: State;

    /// Wrap a raw AST node of this `Kind` + `State` into this view.
    fn from_raw(raw: AstNodeWrapper<Self::Kind, Self::State, <Self::Kind as Kind>::Syntax>)
    -> Self;
}

impl<K, S, Sy> View for AstNodeWrapper<K, S, Sy>
where
    K: NodeKind<Syntax = Sy>,
    Sy: Syntax,
    S: State,
{
    type Kind = K;
    type State = S;

    fn from_raw(raw: AstNodeWrapper<K, S, Sy>) -> Self {
        raw
    }
}

pub trait NodeKind: Kind {
    type View<S: State>: View<Kind = Self, State = S>;
}
pub trait TokenKind: Kind {}

pub trait AstNode: Sized {
    type Kind: NodeKind;
    fn cast(syntax: &SyntaxNode<<Self::Kind as Kind>::Syntax>) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode<<Self::Kind as Kind>::Syntax>;
}

pub trait AstToken: Sized {
    type Syntax: Syntax;
    fn cast(tok: &SyntaxToken<Self::Syntax>) -> Option<Self>;
    fn syntax(&self) -> &SyntaxToken<Self::Syntax>;
    fn text<'resolve>(&self, resolver: &'resolve impl Resolver) -> &'resolve str;
}

impl<K: NodeKind<Syntax = Sy>, S, Sy: Syntax> AstNode for AstNodeWrapper<K, S, Sy> {
    type Kind = K;

    fn cast(s: &SyntaxNode<<Self::Kind as Kind>::Syntax>) -> Option<Self> {
        if K::KINDS.contains(&s.kind()) {
            Some(Self(s.clone(), PhantomData))
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode<<Self::Kind as Kind>::Syntax> {
        &self.0
    }
}

impl<K: TokenKind<Syntax = Sy>, S, Sy: Syntax> AstToken for AstTokenWrapper<K, S, Sy> {
    type Syntax = <K as Kind>::Syntax;
    fn cast(t: &SyntaxToken<Self::Syntax>) -> Option<Self> {
        if K::KINDS.contains(&t.kind()) {
            Some(Self(t.clone(), PhantomData))
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxToken<Self::Syntax> {
        &self.0
    }
    fn text<'resolve>(&self, resolver: &'resolve impl Resolver) -> &'resolve str {
        self.0.resolve_text(resolver)
    }
}

#[inline]
pub fn child<N>(parent: &SyntaxNode<<N::Kind as Kind>::Syntax>) -> Option<N>
where
    N: AstNode,
{
    parent.children().find_map(N::cast)
}

#[inline]
pub fn token<T, Sy>(parent: &SyntaxNode<Sy>) -> Option<T>
where
    T: AstToken<Syntax = Sy>,
    Sy: Syntax,
{
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find_map(T::cast)
}

/// Lift a token child from a node, automatically rebinding its typestate.
///
/// - For Verified, returns K<Verified>
/// - For HasErrors, returns Option<K<Unchecked>>
/// - For Unchecked, panics
#[inline]
pub fn lift_token<S: State, K: TokenKind<Syntax = Sy>, P: NodeKind<Syntax = Sy>, Sy: Syntax>(
    parent: &AstNodeWrapper<P, S, Sy>,
) -> S::Out<AstTokenWrapper<K, S::ChildState, Sy>>
where
    AstTokenWrapper<K, S, Sy>: AstToken<Syntax = Sy>,
    P: RequiredChild<K>,
{
    let opt = token::<AstTokenWrapper<_, S, _>, _>(&parent.0);
    unsafe { S::lift(opt) }
}

/// Lift a node child from a node, automatically rebinding its typestate.
///
/// - For Verified, returns K<Verified>
/// - For HasErrors, returns Option<K<Unchecked>>
/// - For Unchecked, panics

pub fn lift_child<S: State, K: NodeKind<Syntax = Sy>, P: NodeKind<Syntax = Sy>, Sy: Syntax>(
    parent: &AstNodeWrapper<P, S, Sy>,
) -> S::Out<AstNodeWrapper<K, S::ChildState, Sy>>
where
    AstNodeWrapper<K, S, Sy>: AstNode<Kind = K>,
    P: RequiredChild<K>,
{
    let opt = child::<AstNodeWrapper<K, S, Sy>>(&parent.0);

    // SAFETY:
    // - For S = Verified: P: RequiredChild<K> guarantees that every verified
    //   P node always has a K child, so opt is guaranteed to be Some.
    //   This satisfies the precondition of State::lift for Verified.
    // - For S = HasErrors: the HasErrors implementation of State::lift
    //   accepts and returns Option, so passing opt is safe.
    // - For S = Unchecked: the Unchecked implementation of State::lift
    //   is allowed to assume this code is unreachable, calling it may panic
    //   but will not cause UB.
    unsafe { S::lift(opt) }
}
