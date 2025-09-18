mod macros;
mod shape;

pub use shape::*;

use cstree::syntax::{SyntaxNode, SyntaxToken};

use crate::Syntax;

pub trait AstNode {
    type Syntax: Syntax;
    const KINDS: &[Self::Syntax];

    fn can_cast(kind: Self::Syntax) -> bool {
        Self::KINDS.contains(&kind)
    }

    fn cast(syntax: &SyntaxNode<Self::Syntax>) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode<Self::Syntax>;
}

pub trait AstToken {
    type Syntax: Syntax;
    const KINDS: &[Self::Syntax];

    fn can_cast(kind: Self::Syntax) -> bool {
        Self::KINDS.contains(&kind)
    }

    fn cast(syntax: &SyntaxToken<Self::Syntax>) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken<Self::Syntax>;

    fn text(&self) -> &str;
}
