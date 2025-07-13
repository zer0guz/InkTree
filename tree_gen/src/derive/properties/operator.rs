use std::{hash::Hasher, mem::discriminant};

use chumsky::
    pratt::Associativity
;
use derive_more::From;
use enum_dispatch::enum_dispatch;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Expr, Ident, Lit, LitInt, MetaList, punctuated::Punctuated, token::Comma};

use crate::derive::parser::{FromMeta, MetaError};

#[derive(Debug, PartialEq, Eq)]
#[enum_dispatch(PrattOperator)]

pub enum OperatorKind {
    Infix(Infix),
    Prefix(Prefix),
    Postfix(Postfix),
}

pub struct Operator {
    pub kind: OperatorKind,
    pub ident: Ident,
}

impl Operator {
    pub fn pratt_op(&self, atom_ident: &Ident, lang_ident: &Ident) -> TokenStream {
        let op_ident = &self.ident;
        let parser = quote! {#op_ident::parser().with_cp()};
        self.kind.pratt(parser, atom_ident, lang_ident)
    }
}

#[enum_dispatch]
pub trait PrattOperator {
    fn pratt(&self, parser: TokenStream, ident: &Ident, lang_ident: &Ident) -> TokenStream;
}

#[derive(Debug, PartialEq, Eq, Hash, From)]
pub struct Prefix(u16);

#[derive(Debug, PartialEq, Eq, From)]
pub struct Infix(Associativity);

impl std::hash::Hash for Infix {
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(&self.0).hash(state);
        match self.0 {
            Associativity::Left(prec) | Associativity::Right(prec) => {
                prec.hash(state);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Postfix(u16);

impl PrattOperator for Prefix {
    fn pratt(&self, parser: TokenStream, ident: &Ident, lang_ident: &Ident) -> TokenStream {
        let fold = quote! {|lhs, _, extra| {
            let builder: &mut Builder<'_, '_, #lang_ident> = extra.state();
            builder.start_node_at(lhs, #lang_ident::#ident);
            builder.finish_node();
            lhs
        }};
        let prec = self.0;

        quote! {prefix(#prec,#parser,#fold)}
    }
}
impl PrattOperator for Infix {
    fn pratt(&self, parser: TokenStream, ident: &Ident, lang_ident: &Ident) -> TokenStream {
        let fold = quote! {|lhs, _,_, extra| {
            let builder: &mut Builder<'_, '_, #lang_ident> = extra.state();
            builder.start_node_at(lhs, #lang_ident::#ident);
            builder.finish_node();
            lhs
        }};
        let assoc = match self.0 {
            Associativity::Left(prec) => {
                quote! {::tree_gen::chumsky::pratt::Associativity::Left(#prec)}
            }
            Associativity::Right(prec) => {
                quote! {::tree_gen::chumsky::pratt::Associativity::Right(#prec)}
            }
        };

        quote! {infix(#assoc,#parser,#fold)}
    }
}
impl PrattOperator for Postfix {
    fn pratt(&self, parser: TokenStream, ident: &Ident, lang_ident: &Ident) -> TokenStream {
        let fold = quote! {|lhs, _, extra| {
            let builder: &mut Builder<'_, '_, #lang_ident> = extra.state();
            builder.start_node_at(lhs, #lang_ident::#ident);
            builder.finish_node();
            lhs
        }};
        let prec = self.0;

        quote! {postfix(#prec,#parser,#fold)}
    }
}

impl Prefix {
    fn from_lit_int(lit: LitInt) -> Result<Self, MetaError> {
        Ok(Self(lit.base10_parse::<u16>()?.into()))
    }
}

impl Postfix {
    fn from_lit_int(lit: LitInt) -> Result<Self, MetaError> {
        Ok(Self(lit.base10_parse::<u16>()?.into()))
    }
}

impl FromMeta for Prefix {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let lit: LitInt = list.parse_args()?;

        Ok(Self::from_lit_int(lit)?.into())
    }
}
impl FromMeta for Postfix {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
        let lit: LitInt = list.parse_args()?;

        Ok(Self::from_lit_int(lit)?.into())
    }
}

impl FromMeta for Infix {
    fn from_list(list: &MetaList) -> Result<Self, MetaError> {
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
