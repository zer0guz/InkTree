use regex_syntax::hir::Look;
use snafu::Snafu;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]

pub enum SemanticError {
    #[snafu(display("Error converting Hir to Mir"))]
    FromHir { source: syn::Error },

    #[snafu(display("Error only one attribute per variant"))]
    MultipleAttributes { source: syn::Error },
    #[snafu(display("Error converting Hir to Mir"))]
    FromStr,
    #[snafu(display("Error found: {:#?}, lookaround is not supported by this engine", look))]
    Look { look: Look },
}
