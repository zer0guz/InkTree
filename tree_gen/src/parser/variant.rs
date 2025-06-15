use snafu::{OptionExt, ResultExt, Snafu};
use strum::VariantNames;
use syn::{Attribute, Ident, Meta, punctuated::Punctuated, token::Comma};

use crate::properties::{FromMeta, PathNotIdentSnafu, PropertyError, StaticToken, UnsupportedSnafu};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
pub enum VariantError {
    #[snafu(display(
        "attributes have to be provided by placing them into tree_gen() SCOPE TODO: {}",
        source
    ))]
    Scope { source: syn::Error },
    #[snafu(display(
        " attributes have to be provided as a comma seperated list: {}",
        source
    ))]
    Format { source: syn::Error },

    #[snafu(display("Error while parsing the property {}", source))]
    #[snafu(context(false))]
    Property { source: PropertyError },
}

pub struct SyntaxVariant {
    pub ident: Ident,
    pub attributes: Vec<SyntaxAttribute>,
}

impl SyntaxVariant {
    pub fn from_attr(attr: Attribute) -> Result<Self, VariantError> {
        let mut attributes = vec![];
        let list = attr.meta.require_list().context(ScopeSnafu)?;

        let metas: Vec<Meta> = list
            .parse_args_with(Punctuated::<Meta, Comma>::parse_terminated)
            .context(FormatSnafu)?
            .into_iter()
            .collect();

        for meta in metas {
            let a = SyntaxAttribute::from_meta(meta)?;
            attributes.push(a);
        }

        Ok(Self {
            ident: list
                .path
                .get_ident()
                .expect("top level tree_gen() is always an ident")
                .clone(),
            attributes,
        })
    }
}

#[derive(Debug, VariantNames)]
#[strum(serialize_all = "kebab-case")]
pub enum SyntaxAttribute {
    #[strum(serialize = "static_token")]
    StaticToken(StaticToken),
    //Token(Token),
    //Node(Node),
    None(String),
}

impl SyntaxAttribute {
    fn parse(name: &str, meta: Meta) -> Result<Self, PropertyError> {
        match name {
            "static_token" => Ok(StaticToken::from_meta(meta)?.into()),
            #[rustfmt::skip]
            _ => {
                UnsupportedSnafu {
                    source: syn::Error::new_spanned(meta.path(), "Not supported TODO text")
                }
                .fail()
            },
        }
    }
    fn from_meta(meta: Meta) -> Result<Self, PropertyError> {
        let ident = meta.path().get_ident().context(PathNotIdentSnafu {
            source: syn::Error::new_spanned(meta.path(), "Path is not an identifier TODO text"),
        })?;
        let Some(name) = SyntaxAttribute::VARIANTS
            .iter()
            .enumerate()
            .find_map(|(index, name)| {
                if name == &ident.to_string().as_str() {
                    Some(SyntaxAttribute::VARIANTS[index])
                } else {
                    None
                }
            })
        else {
            todo!("not suppoerted todo text")
        };
        SyntaxAttribute::parse(name, meta)
    }
}
