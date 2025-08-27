use std::str::FromStr;

use crate::{
    derive::{
        attributes::{Node, Pratt, Rule, StaticToken, Token},
        parser::FromMeta,
    },
    language::ElementError,
};
use enum_dispatch::enum_dispatch;
use strum::{EnumDiscriminants, EnumString};
use syn::{Ident, Meta};

#[derive(EnumDiscriminants)]
#[strum_discriminants(vis(pub), strum(serialize_all = "snake_case"))]
#[strum_discriminants(name(SyntaxAttributeKind), derive(EnumString))]
#[enum_dispatch(LanguageElement)]

pub enum SyntaxAttribute {
    StaticToken(StaticToken),
    Node(Node),
    Token(Token),
    Pratt(Pratt),
    Rule(Rule),
}

impl SyntaxAttributeKind {
    pub fn from_meta(
        self,
        meta: &Meta,
        name: Option<&Ident>,
    ) -> Result<SyntaxAttribute, ElementError> {
        match self {
            Self::StaticToken => Ok(StaticToken::from_meta(meta, name)?.into()),
            Self::Node => Ok(Node::from_meta(meta, name)?.into()),
            Self::Token => Ok(Token::from_meta(meta, name)?.into()),
            Self::Pratt => Ok(Pratt::from_meta(meta, name)?.into()),
            Self::Rule => Ok(Rule::from_meta(meta, name)?.into()),
        }
    }
}

impl SyntaxAttribute {
    pub fn from_meta(meta: &Meta, name: Option<&Ident>) -> Result<SyntaxAttribute, ElementError> {
        let kind = meta.path().get_ident().expect("asdsad");
        if let Ok(kind) = SyntaxAttributeKind::from_str(&kind.to_string().as_str()) {
            Ok(kind.from_meta(&meta, name)?)
        } else {
            Err(syn::Error::new_spanned(kind, "unsupported todo text").into())
        }
    }
}
