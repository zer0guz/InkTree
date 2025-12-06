use std::fmt::Debug;

use chumsky::{
    Parser,
    error::{EmptyErr, Rich},
};
use cstree::{build::NodeCache, green::GreenNode, interning::MultiThreadedTokenInterner};

use crate::engine::{
    Builder, Parseable, Syntax,
    recovery::{Recovering, Strict},
};
pub trait ParserEngine {
    type Syntax: Syntax;

    fn parse_with_cache<'src, 'interner, 'borrow, 'cache, Err>(
        cache: &'cache mut NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
        input: &'src str,
    ) -> Result<GreenNode, Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + Debug,
        'interner: 'src,
    {
        // ---------- 1st pass: strict ----------
        let strict_result: Result<GreenNode, Err> = {
            // `'cache` is inferred as the lifetime of this `&mut` borrow,
            // which is also the lifetime of this block.
            let mut builder = Builder::<Self::Syntax>::with_cache(cache);

            // `'cache` for the parser is also inferred as that same block lifetime (`'_`).
            let parser =
                <Self::Syntax as Syntax>::Root::go::<Err, Strict>();

            let result = parser.parse_with_state(input, &mut builder).into_result();

            match result {
                Ok(_) => {
                    let (green, _cache_back) = builder.finish();
                    Ok(green)
                }
                Err(e) => todo!(),
            }
            // `builder` and `parser` are dropped *here*; the borrow of `cache` ends.
        };

        if let Ok(green) = strict_result {
            return Ok(green);
        }

        // ---------- 2nd pass: recovering ----------
        let mut builder = Builder::<'_, 'interner, 'borrow, Self::Syntax>::with_cache(cache);
        let parser =
            <Self::Syntax as Syntax>::Root::go::<Err, Recovering>();

        let _ = parser.parse_with_state(input, &mut builder).into_result();
        // TODO: collect / attach errors

        let (green, _cache_back) = builder.finish();
        Ok(green)
    }
}

impl<Sy: Syntax> ParserEngine for Sy {
    type Syntax = Sy;
}
