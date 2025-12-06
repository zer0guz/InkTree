use proc_macro2::TokenStream;
use quote::quote;
use snafu::ResultExt;
use syn::{Expr, ExprCall, Ident, MetaList, ExprPath,};

use crate::{derive::parser::FromMeta, language::ElementError};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Recover {
    pub kind: RecoverKind,
}
impl Recover {
    pub(crate) fn codegen(&self) -> TokenStream {
        match &self.kind {
            // skip_past(Semicolon)
            RecoverKind::SkipPast { term } => {
                quote! { ::inktree::engine::recovery::SkipPast<#term> }
            }

            // skip_to(Semicolon) / skip_to(Comma, RBrace)
            RecoverKind::SkipTo { stops } => {
                match stops.as_slice() {
                    [] => {
                        // You can also choose to default or error here.
                        // For now: hard error so misuse is obvious.
                        panic!("recover(skip_to(...)) requires at least one stop token");
                    }
                    [single] => {
                        // Single stop -> treat as a 1-tuple so TokenSet impl can be shared
                        quote! {
                            ::inktree::engine::recovery::SkipTo<(#single,)>
                        }
                    }
                    many => {
                        // Multiple stops -> tuple of token types, e.g. (Comma, RBrace)
                        quote! {
                            ::inktree::engine::recovery::SkipTo<(#(#many),*)>
                        }
                    }
                }
            }

            // list(Comma, RParen)  — classic “list items with a separator and an end token”
            RecoverKind::List { sep, end } => {
                quote! {
                    ::inktree::engine::recovery::List<#sep, #end>
                }
            }

            // balanced(LParen, RParen) — for nested parentheses/braces/etc
            RecoverKind::Balanced { open, close } => {
                quote! {
                    ::inktree::engine::recovery::Balanced<#open, #close>
                }
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecoverKind {
    /// `recover(skip_to(Semicolon))` or `recover(skip_to(Semicolon, RBrace))`
    SkipTo { stops: Vec<Ident> },

    /// `recover(skip_past(Semicolon))`
    SkipPast { term: Ident },

    /// `recover(list(Comma, ParenClose))`
    List { sep: Ident, end: Ident },

    /// `recover(balanced(LParen, RParen))`
    Balanced { open: Ident, close: Ident },
}


impl FromMeta for Recover {
    fn from_path(path: &syn::Path, _name: Option<&Ident>) -> Result<Self, ElementError> {
        // If you want to *forbid* bare `recover`:
        Err(syn::Error::new_spanned(
            path,
            "expected `recover(<strategy>(...))`, e.g. `recover(skip_to(Semicolon))`",
        ))
        .context(crate::language::MetaSnafu)?
    }

    fn from_list(list: &MetaList, _name: Option<&Ident>) -> Result<Self, ElementError> {
        use syn::spanned::Spanned;

        // Everything inside `recover(...)` is parsed as a single Expr:
        //
        //   recover(skip_to(Semicolon, RBrace))
        //          ^^^^^^^^^^^^^^^^^^^^^^^^^^ this Expr
        //
        let expr: Expr = list.parse_args()?;

        let Expr::Call(ExprCall { func, args, .. }) = expr else {
            return Err(syn::Error::new_spanned(
                expr,
                "expected a recovery strategy call like `skip_to(Semicolon)`",
            ))
            .context(crate::language::MetaSnafu)?;
        };

        // Extract strategy name: skip_to, skip_past, list, balanced, ...
        let func_ident = match *func {
            Expr::Path(ExprPath { path, .. }) => path
                .get_ident()
                .cloned()
                .ok_or_else(|| {
                    syn::Error::new_spanned(
                        path,
                        "expected a simple identifier (e.g. `skip_to`, `skip_past`, `list`, `balanced`)",
                    )
                })?,
            other => {
                return Err(syn::Error::new_spanned(
                    other,
                    "expected a recovery strategy name like `skip_to`, `skip_past`, `list`, or `balanced`",
                ))
                .context(crate::language::MetaSnafu)?;
            }
        };

        let strategy_name = func_ident.to_string();

        // Convert args into Idents (token names like `Semicolon`, `Comma`, `ParenClose`, etc.)
        let mut idents = Vec::<Ident>::new();
        for arg in args {
            match arg {
                Expr::Path(ExprPath { path, .. }) => {
                    let ident = path
                        .get_ident()
                        .cloned()
                        .ok_or_else(|| {
                            syn::Error::new_spanned(
                                path,
                                "expected a token name identifier (e.g. `Semicolon`)",
                            )
                        })?;
                    idents.push(ident);
                }
                other => {
                    return Err(syn::Error::new_spanned(
                        other,
                        "expected token name identifiers (e.g. `Semicolon`, `RBrace`)",
                    ))
                    .context(crate::language::MetaSnafu)?;
                }
            }
        }

        let kind = match strategy_name.as_str() {
            // skip_to(Semicolon[, RBrace, ...])
            "skip_to" => {
                if idents.is_empty() {
                    return Err(syn::Error::new(
                        list.span(),
                        "`skip_to` requires at least one token name: `recover(skip_to(Semicolon))`",
                    ))
                    .context(crate::language::MetaSnafu)?;
                }

                RecoverKind::SkipTo { stops: idents }
            }

            // skip_past(Semicolon)
            "skip_past" | "skip_until" => {
                if idents.len() != 1 {
                    return Err(syn::Error::new(
                        list.span(),
                        "`skip_past` requires exactly one token name: `recover(skip_past(Semicolon))`",
                    ))
                    .context(crate::language::MetaSnafu)?;
                }

                RecoverKind::SkipPast { term: idents.into_iter().next().unwrap() }
            }

            // list(Comma, ParenClose)
            "list" => {
                if idents.len() != 2 {
                    return Err(syn::Error::new(
                        list.span(),
                        "`list` requires two token names: `recover(list(Comma, ParenClose))`",
                    ))
                    .context(crate::language::MetaSnafu)?;
                }

                let [sep, end] = <[_; 2]>::try_from(idents).unwrap();
                RecoverKind::List { sep, end }
            }

            // balanced(ParenOpen, ParenClose)
            "balanced" => {
                if idents.len() != 2 {
                    return Err(syn::Error::new(
                        list.span(),
                        "`balanced` requires two token names: `recover(balanced(ParenOpen, ParenClose))`",
                    ))
                    .context(crate::language::MetaSnafu)?;
                }

                let [open, close] = <[_; 2]>::try_from(idents).unwrap();
                RecoverKind::Balanced { open, close }
            }

            other => {
                return Err(syn::Error::new_spanned(
                    func_ident,
                    format!(
                        "unknown recovery strategy `{}` (expected `skip_to`, `skip_past`, \
                         `list`, or `balanced`)",
                        other
                    ),
                ))
                .context(crate::language::MetaSnafu)?;
            }
        };

        Ok(Recover { kind })
    }
}
