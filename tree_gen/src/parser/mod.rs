mod syntax_enum;
mod variant;

pub use syntax_enum::SyntaxEnum;
pub use variant::SyntaxVariant;

use crate::parser::variant::VariantError;
use snafu::Snafu;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]
pub enum SyntaxError {
    #[snafu(display("the variant :{}", source))]
    #[snafu(context(false))]
    Single { source: VariantError },

    #[snafu(display("Error: Repr todo text "))]
    Repr {
        message: &'static str,
        source: syn::Error,
    },
    #[snafu(display("Error: Repr todo text "))]
    #[snafu(context(false))]
    Data { source: syn::Error },
}
