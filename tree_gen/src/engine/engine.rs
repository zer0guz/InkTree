use std::fmt::Debug;

use chumsky::Parser;
use cstree::{
    build::{GreenNodeBuilder, NodeCache},
    green::GreenNode,
    interning::MultiThreadedTokenInterner,
};

use crate::{
    engine::Builder,
    language::{Parseable, Syntax},
};

pub fn parse_with_cache<'src, 'interner, 'borrow, 'parse, Tok, Err, Sy>(
    cache: NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
    input: &'src str,
) -> Result<GreenNode, Err>
where
    Err: chumsky::error::Error<'src, &'src str> + 'src + Debug + 'src,
    Tok: Parseable<Syntax = Sy>,
    Sy: Syntax,
    'borrow: 'src,
    'interner: 'src,
{
    let mut builder: Builder<'_, 'interner, 'borrow, Sy> = Builder {
        builder: GreenNodeBuilder::<Sy, &MultiThreadedTokenInterner>::from_cache(cache),
    };

    let parser = Tok::parser::<Err>();
    let result = parser.parse_with_state(input, &mut builder);

    result.unwrap();

    let (green, _) = builder.finish();
    Ok(green)
}
