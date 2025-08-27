use std::marker::PhantomData;

use cstree::{build::NodeCache, green::GreenNode};

use crate::{
    chumksy_ext::Input,
    language::{Parseable, Syntax},
};

pub struct _SyntaxEngine<'interner, Sy> {
    cache: NodeCache<'interner>,
    _phantom: PhantomData<Sy>,
}

impl<'interner, Sy> _SyntaxEngine<'interner, Sy>
where
    Sy: Syntax,
{
    pub fn _new() -> Self {
        Self {
            cache: NodeCache::new(),
            _phantom: PhantomData,
        }
    }
    pub fn _from_cache(cache: NodeCache<'interner>) -> Self {
        Self {
            cache: cache,
            _phantom: PhantomData,
        }
    }

    fn _parse<'src, Err, Tok>(&mut self, _input: Input<'src>) -> Result<GreenNode, Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        Tok: Parseable<Syntax = Sy>,
        'interner: 'src,
    {
        //let mut builder = Builder::with_cache(&mut self.cache);

        // Tok::parser::<Err>()
        //     .parse_with_state(input, &mut builder);

        // let (green, _) = builder.finish();
        // Ok(green)

        todo!("engine parse")
    }
}
