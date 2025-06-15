mod meta;
mod node;
mod static_token;

pub use meta::{FromMeta, MetaError};
use snafu::Snafu;
pub use static_token::StaticToken;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
pub enum PropertyError {
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
    Unsupported {
        #[snafu(source(false))]
        source: syn::Error,
    },
}
