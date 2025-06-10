use chumsky::{extra::{Full, ParserExtra}, inspector::Inspector};
use cstree::Syntax;

use crate::chumksy::{state::{BuilderState, InspectorBuilderState}, Input};


/// The one-and-only “extra” we use:
pub type GreenExtra<'cache, 'interner, Err, Sy> =
    Full<Err, InspectorBuilderState<'cache, 'interner, Sy>, ()>;

pub trait BuilderExtra<'src>: ParserExtra<'src, Input<'src>> {
    type Error: chumsky::error::Error<'src, &'src str>;
    type Builder: BuilderState<'src> + Inspector<'src, Input<'src>>;
}

impl<'src, 'cache, 'interner, Err, Sy> BuilderExtra<'src> for GreenExtra<'cache, 'interner, Err, Sy>
where
    Sy: Syntax + Copy + 'src,
    Err: chumsky::error::Error<'src, &'src str> + 'src,
    'cache: 'src,
{
    type Error = Err;
    type Builder = InspectorBuilderState<'cache, 'interner, Sy>;
}
