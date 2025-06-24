mod error;
//mod generator;
//mod mir;
mod element;
mod language;
//mod implementations;

pub use element::{LanguageElement,verify_properties};
pub use error::*;
pub use language::Language;
