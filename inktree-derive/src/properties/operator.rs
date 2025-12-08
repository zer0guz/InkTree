use std::{hash::Hasher, mem::discriminant};

use chumsky::pratt::Associativity;
use derive_more::From;
use enum_dispatch::enum_dispatch;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Expr, Ident, Lit, LitInt, MetaList, punctuated::Punctuated, token::Comma};

use crate::{language::ElementError, parser::FromMeta};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[enum_dispatch(PrattOperator)]

pub enum OperatorKind {
    Infix(Infix),
    Prefix(Prefix),
    Postfix(Postfix),
}

#[derive(Debug)]

pub struct Operator {
    pub kind: OperatorKind,
    pub ident: Ident,
}

impl Operator {
    pub fn pratt_op(&self) -> TokenStream {
        let op_ident = &self.ident;
        let parser = quote! {#op_ident::parser().with_cp()};
        self.kind.pratt(parser)
    }
    pub fn is_prefix(&self) -> bool {
        matches!(self.kind, OperatorKind::Prefix(_))
    }
    pub fn is_infix(&self) -> bool {
        matches!(self.kind, OperatorKind::Infix(_))
    }
    pub fn is_postfix(&self) -> bool {
        matches!(self.kind, OperatorKind::Postfix(_))
    }
}

#[enum_dispatch]
pub trait PrattOperator {
    fn pratt(&self, parser: TokenStream) -> TokenStream;
}

#[derive(Debug, PartialEq, Eq, Hash, From, Clone, Copy)]
pub struct Prefix(u16);

#[derive(Debug, PartialEq, Eq, From, Clone, Copy)]
pub struct Infix(Associativity);

impl std::hash::Hash for Infix {
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(&self.0).hash(state);
        match self.0 {
            Associativity::Left(prec) | Associativity::Right(prec) => {
                prec.hash(state);
            }
            Associativity::None(_) => todo!("chumsky pratt assoc none update todo"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Postfix(u16);

impl PrattOperator for Prefix {
    fn pratt(&self, parser: TokenStream) -> TokenStream {
        let prec = self.0;

        quote! {
            (#prec, #parser)
        }
    }
}
impl PrattOperator for Infix {
    fn pratt(&self, parser: TokenStream) -> TokenStream {
        match self.0 {
            Associativity::Left(prec) => {
                quote! {(#prec,Left,#parser)}
            }
            Associativity::Right(prec) => {
                quote! {(#prec,Left,#parser)}
            }
            Associativity::None(_) => {
                todo!("handle chumskys new None assoc");
                //quote! {#prec,None,#parser}
            }
        }
    }
}
impl PrattOperator for Postfix {
    fn pratt(&self, parser: TokenStream) -> TokenStream {
        let prec = self.0;

        quote! {(#prec,#parser)}
    }
}

impl Prefix {
    fn from_lit_int(lit: LitInt) -> Result<Self, ElementError> {
        Ok(Self(lit.base10_parse::<u16>()?.into()))
    }
}

impl Postfix {
    fn from_lit_int(lit: LitInt) -> Result<Self, ElementError> {
        Ok(Self(lit.base10_parse::<u16>()?.into()))
    }
}

impl FromMeta for Prefix {
    fn from_list(list: &MetaList, _name: Option<&Ident>) -> Result<Self, ElementError> {
        let lit: LitInt = list.parse_args()?;

        Ok(Self::from_lit_int(lit)?.into())
    }
}
impl FromMeta for Postfix {
    fn from_list(list: &MetaList, _name: Option<&Ident>) -> Result<Self, ElementError> {
        let lit: LitInt = list.parse_args()?;

        Ok(Self::from_lit_int(lit)?.into())
    }
}

impl FromMeta for Infix {
    fn from_list(list: &MetaList, _name: Option<&Ident>) -> Result<Self, ElementError> {
        if let Some((assoc, prec)) = list
            .parse_args_with(Punctuated::<Expr, Comma>::parse_terminated)?
            .into_iter()
            .collect_tuple()
        {
            match (assoc, prec) {
                (Expr::Path(assoc), Expr::Lit(prec)) => {
                    let prec = match prec.lit {
                        Lit::Int(lit_int) => lit_int.base10_parse::<u16>()?,
                        _ => todo!(),
                    };
                    match assoc.path.require_ident()?.to_string().as_str() {
                        "left" => Ok(Associativity::Left(prec).into()),
                        "right" => Ok(Associativity::Right(prec).into()),
                        _ => todo!("assoc not left nor right"),
                    }
                }
                _ => todo!("argument types infix"),
            }
        } else {
            todo!("something something")
        }
    }
}
