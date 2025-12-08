use syn::Ident;

use crate::{language::ElementError, parser::FromMeta};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Ast {
    Ignored,
}

impl FromMeta for Ast {
    fn from_list(list: &syn::MetaList, _: Option<&Ident>) -> Result<Self, ElementError> {
        let option: Ident = list.parse_args()?;
        match option.to_string().as_str() {
            "ignore" => Ok(Self::Ignored),

            _ => Err(syn::Error::new_spanned(
                list,
                "dont even know why we return Result instead of option here...",
            )
            .into()),
        }
    }
}

// /// Implemented by all generated AST node wrappers.
// pub trait AstNode<Sy: Syntax>: Sized {
//     /// Check if this node can be cast from a raw syntax node.
//     fn can_cast(kind: Sy) -> bool;

//     /// Cast a raw syntax node into this wrapper, if the kind matches.
//     fn cast(syntax: SyntaxNode<Sy>) -> Option<Self>;

//     /// Get back to the underlying syntax node.
//     fn syntax(&self) -> &SyntaxNode<Sy>;
// }/// Implemented by all generated AST node wrappers.
