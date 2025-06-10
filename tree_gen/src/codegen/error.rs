use regex_syntax::hir::Look;
use snafu::Snafu;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(super)))]

pub enum CodegenError {
    #[snafu(display("Error converting Hir to Mir"))]
    FromHir { source: regex_syntax::Error },
    #[snafu(display("Error converting Hir to Mir"))]
    FromStr,
    #[snafu(display("Error found: {:#?}, lookaround is not supported by this engine", look))]
    Look { look: Look },
}
