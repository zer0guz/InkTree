mod error;
//mod generator;
//mod mir;
mod element;
mod language;
mod structs;
//mod implementations;

pub use element::{Verify,LanguageElement};
pub use error::SemanticError;
pub use language::Language;
