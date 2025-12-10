use chumsky::{Parser, extra::ParserExtra, prelude::todo};


pub trait Parseable: Sized
where
{

    fn parser<'src,Extra>() -> impl Parser<'src,&'src str,(),Extra> + Clone
    where Extra: ParserExtra<'src,&'src str>
    {
        todo()
    }
}