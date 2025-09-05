mod extra;
mod parser;

//-------EXPORTS--------

pub use extra::{GreenExtra, GreenState};
pub use parser::{BuilderParser, ranges, with_cp_ext};

pub type Input<'src> = &'src str;
