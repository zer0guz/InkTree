use chumsky::{
    ParseResult, Parser,
    extra::{Full, ParserExtra},
    input::Cursor,
    inspector::Inspector,
    prelude::todo,
};
use cstree::build::GreenNodeBuilder;

use crate::{
    chumksy_ext::{GreenExtra, GreenState, Input}, language::{Parseable, Syntax}
};

pub trait LanguageBuilder<'cache, 'interner>: Sized
where
    Self::Syntax: Syntax + 'static,
    'interner: 'cache,
{
    type Syntax;

    fn parser<'src, Err, Tok>()
    -> impl Parser<'src, Input<'src>, (), Full<Err, Builder<'cache, 'interner, Self::Syntax>, ()>>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        Tok: Parseable<Syntax = Self::Syntax>,
        'cache: 'src,
    {
        Tok::parser::<Err>()
    }

    fn parse<'src, Err, Tok>(&mut self, input: Input<'src>) -> ParseResult<(), Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        Tok: Parseable<Syntax = Self::Syntax>,
        'cache: 'src;
}

impl<'cache, 'interner, Sy> LanguageBuilder<'cache, 'interner> for Builder<'cache, 'interner, Sy>
where
    Sy: Syntax + 'static,
    'interner: 'cache,
{
    type Syntax = Sy;

    fn parse<'src, Err, Tok>(&mut self, input: Input<'src>) -> ParseResult<(), Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + 'src,
        Tok: Parseable<Syntax = Self::Syntax>,
        'cache: 'src,
    {
        Self::parser::<Err, Tok>().parse_with_state(input, self)
    }
}

pub struct Builder<'cache, 'interner, Sy>
where
    Sy: Syntax,
    'interner: 'cache,
{
    builder: GreenNodeBuilder<'cache, 'interner, Sy>,
}

impl<'cache, 'interner, Sy> Default for Builder<'static, 'static, Sy>
where
    Sy: Syntax,
{
    fn default() -> Self {
        Self {
            builder: GreenNodeBuilder::default(),
        }
    }
}

impl<'src, 'cache, 'interner, Sy> Inspector<'src, Input<'src>> for Builder<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    type Checkpoint = cstree::build::Checkpoint;

    fn on_token(&mut self, _: &char) {}

    fn on_save<'parse>(&self, _: &Cursor<'src, 'parse, Input<'src>>) -> Self::Checkpoint {
        self.builder.checkpoint()
    }

    fn on_rewind<'parse>(
        &mut self,
        marker: &chumsky::input::Checkpoint<'src, 'parse, Input<'src>, Self::Checkpoint>,
    ) {
        self.builder.revert_to(*marker.inspector());
    }
}

impl<'src, 'cache, 'interner, Sy> GreenState<'src> for Builder<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    type SyntaxKind = Sy;

    fn start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Self::SyntaxKind) {
        self.builder.start_node_at(checkpoint, kind);
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn revert_to(&mut self, checkpoint: Self::Checkpoint) {
        self.builder.revert_to(checkpoint);
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        self.builder.checkpoint()
    }

    fn token(&mut self, kind: Self::SyntaxKind, slice: &'src str) {
        self.builder.token(kind, slice);
    }

    fn static_token(&mut self, kind: Self::SyntaxKind) {
        self.builder.static_token(kind);
    }
}
