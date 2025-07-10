mod node;
mod root;
mod static_token;
mod token;

use std::collections::HashMap;

use enum_dispatch::enum_dispatch;
use proc_macro2::TokenStream;
use snafu::{ResultExt, Snafu};
use strum::{EnumDiscriminants, EnumString, IntoDiscriminant};
use syn::{Ident, Meta};
use crate::{derive::{ast::{AttributeOrProperty}, parser::{FromMeta, MetaError}, properties::{Property, PropertyKind}}, util::IteratorExt, Errors};






pub use static_token::*;
pub use node::*;
pub use token::*;
pub use root::*;



#[derive(Debug, Snafu)]
#[snafu(visibility(pub(self)))]
pub enum AttributeError {
    #[snafu(display("'{}': unknown property! expected one of: {}",source.to_string(), "todo!"))]
    Unsupported { source: syn::Error },
}

#[derive(EnumDiscriminants)]
#[strum_discriminants(vis(pub), strum(serialize_all = "snake_case"))]
#[strum_discriminants(name(AttributeKind), derive(EnumString))]
#[enum_dispatch(LanguageElement)]
pub enum Attribute {
    StaticToken(StaticToken),
    Node(Node),
    Token(Token),
    Root(Root),
}


impl AttributeKind {
    pub fn from_meta(self, meta: &Meta) -> Result<Attribute, MetaError> {
        match self {
            Self::StaticToken => Ok(StaticToken::from_meta(meta)?.into()),
            Self::Root => Ok(Root::from_meta(meta)?.into()),
            Self::Node => Ok(Node::from_meta(meta)?.into()),
            Self::Token => Ok(Token::from_meta(meta)?.into()),
        }
    }
}



#[enum_dispatch]
pub trait LanguageElement: Sized {
    fn codegen(&self, ident: &Ident, lang_ident: &Ident,idents: &HashMap<String,Ident>) -> Result<TokenStream, AttributeError>;
    fn verify(
        &self,
        properties: &Vec<Property>,
        ident: &Ident,
    ) -> Result<(), Errors<AttributeError>> {
        properties
            .iter()
            .map(|prop| {
                self.allowed()
                    .contains(&prop.discriminant())
                    .then_some(prop)
                    .ok_or(syn::Error::new_spanned(ident, "todo text"))
                    .context(UnsupportedSnafu)
            })
            .collect_either()?;

        Ok(())
    }
    fn allowed(&self) -> &'static [PropertyKind];
}

