
use std::fmt::Debug;

use chumsky::Parser;
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
        let strict_result: Result<GreenNode, ()> = {
            let mut builder = Builder::with_cache(cache);

            let parser = <Self::Syntax as Syntax>::Root::go::<Err, Strict>();

            let result = parser.parse_with_state(input, &mut builder).into_result();

            match result {
                Ok(_) => {
                    let (green, _cache_back) = builder.finish();
                    Ok(green)
                }
                Err(_e) => Err(()),
            }
        };

        if let Ok(green) = strict_result {
            return Ok(green);
        }
        let mut builder = Builder::with_cache(cache);
        eprintln!("first parse failed trying recovery\n");
        let parser = <Self::Syntax as Syntax>::Root::go::<Err, Recovering>();

        let _ = parser.parse_with_state(input, &mut builder).into_result();
        // TODO nodes of recovery... builder.finish_node();
        // TODO: collect / attach errors

        let (green, _cache_back) = builder.finish();
        Ok(green)
    }
}

impl<Sy: Syntax> ParserEngine for Sy {
    type Syntax = Sy;
}
