mod syntax_enum;
mod variant;

pub use syntax_enum::SyntaxEnum;
pub use variant::SyntaxAttribute;

use crate::parser::variant::VariantError;
use snafu::Snafu;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
pub enum ParseError {
    #[snafu(display("the derive target: {}", source))]
    Derive { source: syn::Error },

    #[snafu(display("the variant :{}", source))]
    #[snafu(context(false))]
    Variant { source: VariantError },
}
