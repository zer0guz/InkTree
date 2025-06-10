use snafu::Snafu;

use crate::codegen::CodegenError;

#[derive(Debug,Snafu)]

pub enum Error {
    #[snafu(display(""))]
    CodeGen {
        source: CodegenError,
    }
}