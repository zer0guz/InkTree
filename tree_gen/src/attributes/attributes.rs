use std::str::FromStr;

use enum_dispatch::enum_dispatch;
use snafu::{OptionExt, ResultExt, Snafu};
use strum::{EnumDiscriminants, EnumString};
use syn::Meta;

use crate::
    attributes::{
        FromMetaKind, MetaError, Node, StaticToken, Token,
        properties::{SyntaxProperty, SyntaxPropertyKind},
    }
;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
pub enum AttributeError {
    #[snafu(display("the attribute {}", source))]
    PathNotIdent {
        #[snafu(source(false))]
        source: syn::Error,
    },

    #[snafu(display("meta text todo {}", source))]
    #[snafu(context(false))]
    Meta { source: MetaError },

    #[snafu(display("syn text todo {}", source))]
    #[snafu(context(false))]
    Syn { source: syn::Error },

    #[snafu(display("'{}': unknown property! expected one of: {}",source.to_string(), "todo!"))]
    Unsupported { source: syn::Error },
}

#[derive(EnumDiscriminants)]
#[strum(serialize_all = "kebab-case")]
#[strum_discriminants(vis(pub))]
#[strum_discriminants(name(SyntaxAttributeKind), derive(EnumString))]
#[enum_dispatch(LanguageElement)]
pub enum SyntaxAttribute {
    #[strum_discriminants(strum(serialize = "static_token"))]
    StaticToken(StaticToken),
    Node(Node),
    Token(Token),
}

impl SyntaxAttributeKind {
    pub fn from_meta(self, meta: &Meta) -> Result<SyntaxAttribute, AttributeError> {
        match self {
            SyntaxAttributeKind::StaticToken => StaticToken::from_meta(meta),
            _ => todo!(),
        }
    }
}

pub enum AttributeOrProperty {
    Attribute(SyntaxAttribute),
    Property(SyntaxProperty),
}

impl AttributeOrProperty {
    pub fn from_meta(meta: Meta) -> Result<Self, AttributeError> {
        let ident = meta.path().get_ident().context(PathNotIdentSnafu {
            source: syn::Error::new_spanned(meta.path(), "Path is not an identifier TODO text"),
        })?;

        if let Ok(kind) = SyntaxAttributeKind::from_str(&ident.to_string().as_str()) {
            Ok(Self::Attribute(kind.from_meta(&meta)?))
        } else if let Ok(kind) = SyntaxPropertyKind::from_str(&ident.to_string().as_str()) {
            Ok(Self::Property(kind.from_meta(&meta)?))
        } else {
            Err(syn::Error::new_spanned(ident, "unsupported todo text")).context(UnsupportedSnafu)
        }
    }
}
