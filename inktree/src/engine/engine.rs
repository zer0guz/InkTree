use std::fmt::Debug;

use chumsky::Parser;
use cstree::{build::NodeCache, green::GreenNode, interning::MultiThreadedTokenInterner};

use crate::{
    engine::Builder,
    language::{Parseable, Syntax},
};
pub fn parse_with_cache<'src, 'cache, 'interner, 'borrow, Tok, Err, Sy>(
    cache: &mut NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
    input: &'src str,
) -> Result<GreenNode, Err>
where
    Err: chumsky::error::Error<'src, &'src str> + Debug,
    Tok: Parseable<Syntax = Sy>,
    Sy: Syntax,
    'interner: 'src,
{
    let green = {
        // builder is local to this block
        let mut builder = Builder::with_cache(cache);

        let parser = Tok::parser::<Err>();
        {
            let result = parser.parse_with_state(input, &mut builder);

            result.unwrap();
        }

        let (green, _returned_cache) = builder.finish();

        // optionally return the cache to caller here...

        green
    };

    Ok(green)
}
