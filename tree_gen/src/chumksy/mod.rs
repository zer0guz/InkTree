mod extra;
mod parser;
//mod token;

use crate::chumksy::extra::GreenExtra;
use chumsky::extra::Full;

//-------EXPORTS--------

pub use extra::BuilderExtra;
pub use parser::BuilderParser;

pub(crate) type Input<'src> = &'src str;

pub type CstreeExtra<'cache, 'interner, Err, Sy> = Full<
    Err,
    GreenExtra<
        cstree::build::GreenNodeBuilder<'cache, 'interner, Sy>,
        Sy,
        cstree::build::Checkpoint,
    >,
    (),
>;
