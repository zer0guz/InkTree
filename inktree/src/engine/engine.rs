use std::fmt::Debug;

use chumsky::Parser;
use cstree::{build::NodeCache, green::GreenNode, interning::MultiThreadedTokenInterner};

use crate::{
    KindMarker, Language, Parseable, Syntax,
    chumsky_ext::Builder,
    engine::recovery::{Recovering, Strict},
};
pub trait ParserEngine {
    fn parse_with_cache<
        'src,
        'interner,
        'borrow,
        'cache,
        const N: usize,
        const ROOT: u16,
        Err,
        L: Language,
    >(
        cache: &'cache mut NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
        input: &'src str,
    ) -> Result<GreenNode, Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + Debug,
        'interner: 'src,
    {
        // let strict_result: Result<GreenNode, ()> = {
        //     let mut builder = Builder::with_cache(cache);

        //     let parser = KindMarker::<{ROOT},N,L>::parser();;

        //     let result = parser.parse_with_state(input, &mut builder).into_result();

        //     match result {
        //         Ok(_) => {
        //             let (green, _cache_back) = builder.finish();
        //             Ok(green)
        //         }
        //         Err(_e) => Err(()),
        //     }
        // };

        // if let Ok(green) = strict_result {
        //     return Ok(green);
        // }
        // let mut builder = Builder::with_cache(cache);
        // eprintln!("first parse failed trying recovery\n");
        // let parser = <Self::Syntax as Syntax>::Root::go::<Err, Recovering>();

        // let _ = parser.parse_with_state(input, &mut builder).into_result();
        // // TODO nodes of recovery... builder.finish_node();
        // // TODO: collect / attach errors

        // let (green, _cache_back) = builder.finish();
        // Ok(green)
        todo!()
    }
}

// impl<L: Language> ParserEngine for Sy {
//     type Syntax = Sy;
// }
