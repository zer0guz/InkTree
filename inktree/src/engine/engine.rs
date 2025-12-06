use std::fmt::Debug;

use chumsky::{Parser, error::EmptyErr};
use cstree::{build::NodeCache, green::GreenNode, interning::MultiThreadedTokenInterner};

use crate::engine::{Builder, Parseable, Syntax, recovery::Recovering};

pub trait ParserEngine {
    type Syntax: Syntax;

    fn parse_with_cache<'src, 'cache, 'interner, 'borrow, Err>(
        cache: &mut NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
        input: &'src str,
    ) -> Result<GreenNode, Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + Debug,
        'interner: 'src,
    {
        let mut builder = Builder::with_cache(cache);
        

        let parser = <Self::Syntax as Syntax>::Root::parser::<EmptyErr>();
        let result = parser.parse_with_state(input, &mut builder);

        let green = match result.into_result(){
            Ok(_) => {
                 let (green, _returned_cache) = builder.finish();
                 green
            },
            Err(err) => {
                //let parser = <Self::Syntax as Syntax>::Root::go::<EmptyErr,Recovering>();
                todo!()
            },
        }

       ;

        Ok(green)
    }
}

impl<Sy: Syntax> ParserEngine for Sy {
    type Syntax = Sy;
}
