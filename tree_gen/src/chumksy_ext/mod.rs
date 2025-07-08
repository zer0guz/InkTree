mod extra;
mod parser;
//mod token;

//-------EXPORTS--------

pub use extra::{GreenExtra, GreenState};
pub use parser::BuilderParser;

pub type Input<'src> = &'src str;
