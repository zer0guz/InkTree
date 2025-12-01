use std::marker::PhantomData;

use crate::{AstNode, AstNodeWrapper, AstTokenWrapper, NodeKind, Syntax, View};

#[derive(Debug)]
pub enum Verified {}

#[derive(Debug)]
pub enum HasErrors {}

#[derive(Debug)]
pub enum Unchecked {}

#[derive(Debug)]
pub enum Never {}

pub trait State {
    type Out<T>;
    type ChildState: State;
    unsafe fn lift<T: Rebind<Self::ChildState>>(opt: Option<T>) -> Self::Out<T::With>;
}

impl State for Verified {
    type Out<T> = T;
    type ChildState = Verified;

    unsafe fn lift<T: Rebind<Self::ChildState>>(opt: Option<T>) -> T::With {
        debug_assert!(opt.is_some());
        unsafe { opt.unwrap_unchecked().rebind() }
    }
}

impl State for HasErrors {
    type Out<T> = Option<T>;
    type ChildState = Unchecked;

    unsafe fn lift<T: Rebind<Self::ChildState>>(opt: Option<T>) -> Option<T::With> {
        opt.map(|t| t.rebind())
    }
}

impl State for Unchecked {
    type Out<T> = Never;
    type ChildState = Unchecked;

    unsafe fn lift<T: Rebind<Self::ChildState>>(_: Option<T>) -> Never {
        unreachable!()
    }
}

pub unsafe trait Verify: Sized + AstNode {
    fn has_errors(&self) -> bool {
        self.syntax().get_data().is_some()
    }

    fn verify(
        self,
    ) -> Result<<Self::Kind as NodeKind>::View<Verified>, <Self::Kind as NodeKind>::View<HasErrors>>
    {
        if <Self as Verify>::has_errors(&self) {
            Err(<Self as Verify>::into_has_errors(self))
        } else {
            // SAFETY: guarded by has_errors() above.
            Ok(unsafe { <Self as Verify>::assume_verified(self) })
        }
    }

    /// # Safety
    /// Call only when `!self.has_errors()`.
    unsafe fn assume_verified(self) -> <Self::Kind as NodeKind>::View<Verified>;

    fn into_has_errors(self) -> <Self::Kind as NodeKind>::View<HasErrors>;
}

// Provide the impl for nodes with `Unchecked` state:
unsafe impl<K, Sy> Verify for AstNodeWrapper<K, Unchecked, Sy>
where
    K: NodeKind<Syntax = Sy>,
    Sy: Syntax,
{
    unsafe fn assume_verified(self) -> K::View<Verified> {
        let raw_verified: AstNodeWrapper<K, Unchecked, Sy> = self.rebind();
        <K::View<Verified> as View>::from_raw_node(raw_verified.0)
    }

    fn into_has_errors(self) -> K::View<HasErrors> {
        let raw_err: AstNodeWrapper<K, Unchecked, Sy> = self.rebind();
        <K::View<HasErrors> as View>::from_raw_node(raw_err.0)
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
