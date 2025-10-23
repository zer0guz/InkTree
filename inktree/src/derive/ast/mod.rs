mod macros;
mod shape;

pub use shape::*;

use crate::Syntax;

#[derive(Clone, Copy, Debug)]

pub struct ParseError;

pub type SyntaxNode<Sy> = cstree::syntax::SyntaxNode<Sy, ParseError>;
pub type SyntaxToken<Sy> = cstree::syntax::SyntaxToken<Sy, ParseError>;

#[inline]
pub fn child<N, Sy>(parent: &SyntaxNode<Sy>) -> Option<N>
where
    N: AstNode<Syntax = Sy>,
    Sy: Syntax,
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

use core::{marker::PhantomData, ops::Deref};

pub struct AstNodeWrapper<K, S, Sy: Syntax>(pub SyntaxNode<Sy>, PhantomData<(K, S)>);
pub struct AstTokenWrapper<K, S, Sy: Syntax>(pub SyntaxToken<Sy>, PhantomData<(K, S)>);

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

pub trait Rebind<Target>: Sized {
    type With;
    fn rebind(self) -> Self::With;
}

impl<K, S, Sy: Syntax, Target> Rebind<Target> for AstNodeWrapper<K, S, Sy> {
    type With = AstNodeWrapper<K, Target, Sy>;
    fn rebind(self) -> Self::With {
        AstNodeWrapper(self.0, PhantomData)
    }
}
impl<K, S, Sy: Syntax, Target> Rebind<Target> for AstTokenWrapper<K, S, Sy> {
    type With = AstTokenWrapper<K, Target, Sy>;
    fn rebind(self) -> Self::With {
        AstTokenWrapper(self.0, PhantomData)
    }
}

pub enum Verified {}
pub enum HasErrors {}
pub enum Unchecked {}
pub enum Never {}

pub trait State {
    type Out<T>;
    type Child: State;
    fn lift<T: Rebind<Self::Child>>(opt: Option<T>) -> Self::Out<T::With>;
}

impl State for Verified {
    type Out<T> = T;
    type Child = Verified;
    fn lift<T: Rebind<Self::Child>>(opt: Option<T>) -> T::With {
        debug_assert!(opt.is_some());
        unsafe { opt.unwrap_unchecked().rebind() }
    }
}
impl State for HasErrors {
    type Out<T> = Option<T>;
    type Child = Unchecked;
    fn lift<T: Rebind<Self::Child>>(opt: Option<T>) -> Option<T::With> {
        opt.map(|t| t.rebind())
    }
}
impl State for Unchecked {
    type Out<T> = Never;
    type Child = Unchecked;
    fn lift<T: Rebind<Self::Child>>(_: Option<T>) -> Never {
        unreachable!()
    }
}

/// Safety: `KINDS` must list exactly the syntax kinds this marker represents.
/// Mismatches can violate verified invariants in your accessors.
pub unsafe trait Kind {
    type Syntax: Syntax;
    const KINDS: &'static [Self::Syntax];
}

pub trait NodeKind: Kind {}
pub trait TokenKind: Kind {}

pub trait AstNode: Sized {
    type Syntax: Syntax;
    const KINDS: &'static [Self::Syntax];
    fn cast(syntax: &SyntaxNode<Self::Syntax>) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode<Self::Syntax>;
}

pub trait AstToken: Sized {
    type Syntax: Syntax;
    const KINDS: &'static [Self::Syntax];
    fn cast(tok: &SyntaxToken<Self::Syntax>) -> Option<Self>;
    fn syntax(&self) -> &SyntaxToken<Self::Syntax>;
    fn text(&self) -> &str;
}

impl<K: NodeKind<Syntax = Sy>, S, Sy: Syntax> AstNode for AstNodeWrapper<K, S, Sy> {
    type Syntax = <K as Kind>::Syntax;
    const KINDS: &[Self::Syntax] = K::KINDS;
    fn cast(s: &SyntaxNode<Self::Syntax>) -> Option<Self> {
        if Self::KINDS.contains(&s.kind()) {
            Some(Self(s.clone(), PhantomData))
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode<Self::Syntax> {
        &self.0
    }
}

impl<K: TokenKind<Syntax = Sy>, S, Sy: Syntax> AstToken for AstTokenWrapper<K, S, Sy> {
    type Syntax = <K as Kind>::Syntax;
    const KINDS: &[Self::Syntax] = K::KINDS;
    fn cast(t: &SyntaxToken<Self::Syntax>) -> Option<Self> {
        if Self::KINDS.contains(&t.kind()) {
            Some(Self(t.clone(), PhantomData))
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxToken<Self::Syntax> {
        &self.0
    }
    fn text(&self) -> &str {
        //self.0.text()
        todo!()
    }
}

/// Unsafe constructor interface the lib uses internally.
pub unsafe trait Verify: Sized + AstNode
where
    Self::Verified: AstNode<Syntax = Self::Syntax>,
{
    type Verified;
    type HasErrors;

    /// Default: use the CST data bit (or override in your macro).
    fn has_errors(&self) -> bool {
        self.syntax().get_data().is_some()
    }

    /// # Safety
    /// Call only when `!self.has_errors()`.
    unsafe fn assume_verified(self) -> Self::Verified;

    fn into_has_errors(self) -> Self::HasErrors;
}

// Provide the impl for nodes with `Unchecked` state:
unsafe impl<K: NodeKind<Syntax = Sy>, Sy: Syntax> Verify for AstNodeWrapper<K, Unchecked, Sy> {
    type Verified = AstNodeWrapper<K, Verified, Sy>;
    type HasErrors = AstNodeWrapper<K, HasErrors, Sy>;
    unsafe fn assume_verified(self) -> Self::Verified {
        AstNodeWrapper(self.0, PhantomData)
    }
    fn into_has_errors(self) -> Self::HasErrors {
        AstNodeWrapper(self.0, PhantomData)
    }
}

// Only Unchecked nodes get the `verify()` method:
impl<K: NodeKind<Syntax = Sy>, Sy: Syntax> AstNodeWrapper<K, Unchecked, Sy> {
    #[must_use]
    pub fn verify(
        self,
    ) -> Result<AstNodeWrapper<K, Verified, Sy>, AstNodeWrapper<K, HasErrors, Sy>> {
        if <Self as Verify>::has_errors(&self) {
            Err(<Self as Verify>::into_has_errors(self))
        } else {
            // SAFETY: guarded by has_errors() above.
            Ok(unsafe { <Self as Verify>::assume_verified(self) })
        }
    }
}

/// Lift a token child from a node, automatically rebinding its typestate.
///
/// - For `Verified`, returns `T`
/// - For `HasErrors`, returns `Option<T>`
/// - For `Unchecked`, returns `Never` (unreachable)
#[inline]
pub fn lift_token<S: State, K: TokenKind<Syntax = Sy>, P: NodeKind<Syntax = Sy>, Sy: Syntax>(
    parent: &AstNodeWrapper<P, S, Sy>,
) -> S::Out<AstTokenWrapper<K, S::Child, Sy>>
where
    AstTokenWrapper<K, S, Sy>: AstToken<Syntax = Sy>,
{
    let opt: Option<AstTokenWrapper<K, S, Sy>> = token::<AstTokenWrapper<K, S, Sy>, Sy>(&parent.0);
    S::lift(opt)
}

/// Lift a node child from a node, automatically rebinding its typestate.
///
/// - For `Verified`, returns `T`
/// - For `HasErrors`, returns `Option<T>`
/// - For `Unchecked`, returns `Never` (unreachable)
#[inline]
pub fn lift_child<S: State, K: NodeKind<Syntax = Sy>, P: NodeKind<Syntax = Sy>, Sy: Syntax>(
    parent: &AstNodeWrapper<P, S, Sy>,
) -> S::Out<AstNodeWrapper<K, S::Child, Sy>>
where
    AstNodeWrapper<K, S, Sy>: AstNode<Syntax = Sy>,
{
    let opt: Option<AstNodeWrapper<K, S, Sy>> = child::<AstNodeWrapper<K, S, Sy>, Sy>(&parent.0);
    S::lift(opt)
}
