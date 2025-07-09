use std::str::FromStr;

use chumsky::{
    Parser,
    extra::{Full, ParserExtra},
    prelude::todo,
};
use enum_dispatch::enum_dispatch;
use proc_macro2::TokenStream;
use quote::quote;
use snafu::{OptionExt, ResultExt, Snafu};
use strum::{EnumDiscriminants, EnumString};
use syn::{Ident, Meta};

use crate::{
    attributes::{node::NodeError, token::TokenError}, chumksy_ext::Input, language::attributes::{
        properties::SyntaxPropertyKind, Node, Root, StaticToken, SyntaxProperty, Token
    }, parser::{FromMeta, MetaError}, Builder, BuilderParser, Syntax
};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum AttributeError {
    #[snafu(display("the attribute {}", source))]
    PathNotIdent {
        #[snafu(source(false))]
        source: syn::Error,
    },

    #[snafu(display("meta text todo {}", source))]
    #[snafu(context(false))]
    Meta { source: MetaError },

    #[snafu(display("meta text todo {}", source))]
    #[snafu(context(false))]
    Node { source: NodeError },

        #[snafu(display("meta text todo {}", source))]
    #[snafu(context(false))]
    Token { source: TokenError },

    #[snafu(display("syn text todo {}", source))]
    #[snafu(context(false))]
    Syn { source: syn::Error },

    #[snafu(display("'{}': unknown property! expected one of: {}",source.to_string(), "todo!"))]
    Unsupported { source: syn::Error },
}

#[derive(EnumDiscriminants)]
#[strum_discriminants(vis(pub), strum(serialize_all = "snake_case"))]
#[strum_discriminants(name(SyntaxAttributeKind), derive(EnumString))]
#[enum_dispatch(LanguageElement)]
pub enum SyntaxAttribute {
    StaticToken(StaticToken),
    Node(Node),
    Token(Token),
    Root(Root),
}

impl SyntaxAttributeKind {
    pub fn from_meta(self, meta: &Meta) -> Result<SyntaxAttribute, AttributeError> {
        match self {
            SyntaxAttributeKind::StaticToken => StaticToken::from_meta(meta),
            SyntaxAttributeKind::Root => Root::from_meta(meta),
            SyntaxAttributeKind::Node => Node::from_meta(meta),
            SyntaxAttributeKind::Token => Token::from_meta(meta),
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

pub trait Parseable: Sized
where
    Self::Syntax: Syntax + 'static,
{
    type Syntax;

    fn parser<'src, 'cache, 'interner, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, (), Err, Self::Syntax>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        'cache: 'src,
        'interner: 'src,
        'interner: 'cache;
}

pub fn parseable_impl(parser: TokenStream, ident: &Ident, lang_ident: &Ident) -> TokenStream {
    quote! {
        impl ::tree_gen::attributes::Parseable for #ident {
            type Syntax = TestLang;

            fn parser<'src, 'cache, 'interner, Err>()
            -> impl ::tree_gen::BuilderParser<'src, 'cache, 'interner, (), Err, #lang_ident>
            where
                Err: chumsky::error::Error<'src, &'src str> + 'src,
                'cache: 'src,
                'interner: 'cache,
            {
                #parser
            }
        }
    }
}

