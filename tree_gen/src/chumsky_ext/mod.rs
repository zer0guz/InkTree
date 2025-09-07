mod extra;
mod parser;

//-------EXPORTS--------

pub use extra::{GreenExtra, GreenState};
pub use parser::{BuilderParser, ranges};

pub type Input<'src> = &'src str;
