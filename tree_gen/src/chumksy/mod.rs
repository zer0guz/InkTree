mod extra;
mod parser;
mod state;
mod token;

use chumsky::inspector::Inspector;


pub use extra::GreenExtra;
pub use parser::BuilderParser;

pub use token::just_token;

use crate::chumksy::{extra::BuilderExtra, state::BuilderState};

pub(crate) type Input<'src> = &'src str;

pub(crate) type Sy<'src, E> =
    <<E as BuilderExtra<'src>>::Builder as BuilderState<'src>>::SyntaxKind;
pub(crate) type Cp<'src, E> =
    <<E as BuilderExtra<'src>>::Builder as Inspector<'src, Input<'src>>>::Checkpoint;
