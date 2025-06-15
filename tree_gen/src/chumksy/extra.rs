use std::marker::PhantomData;

use chumsky::{extra::ParserExtra, input::Cursor, inspector::Inspector};
use cstree::{Syntax, build::GreenNodeBuilder};

use crate::chumksy::Input;
pub trait BuilderExtra<'src>: ParserExtra<'src, Input<'src>>
where
    Self: ParserExtra<'src, Input<'src>, State = Self>,
    Self: Inspector<'src, Input<'src>>,
    Self: BuilderState<'src>,
    Self::SyntaxKind: Copy,
{
    type Error: chumsky::error::Error<'src, Input<'src>>;
    type Builder: BuilderState<'src> + Inspector<'src, Input<'src>>;

    fn revert_to(&mut self, checkpoint: Self::Checkpoint);
    fn checkpoint(&self) -> Self::Checkpoint;
    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>);
}

impl<'src, E, Cp> BuilderExtra<'src> for E
where
    E: ParserExtra<'src, Input<'src>, State = E>,
    E: Inspector<'src, Input<'src>, Checkpoint = Cp>,
    E: BuilderState<'src, Checkpoint = Cp>,
{
    type Error = E::Error;
    type Builder = E;

    fn revert_to(&mut self, checkpoint: Self::Checkpoint) {
        Self::revert_to(self, checkpoint);
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        Self::checkpoint(self)
    }

    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>) {
        <Self as BuilderState>::token(self, kind, slice);
    }
}
pub trait BuilderState<'src>: Inspector<'src, Input<'src>>
where
    Self::SyntaxKind: Copy,
{
    type SyntaxKind;

    fn start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Self::SyntaxKind);
    fn finish_node(&mut self);
    fn revert_to(&mut self, checkpoint: Self::Checkpoint);
    fn checkpoint(&self) -> <Self as Inspector<'src, Input<'src>>>::Checkpoint;
    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>);
}

pub struct GreenExtra<B, Sy, Cp>
where
    B: Builder<Sy, Cp>,
{
    pub builder: B,
    _phantom: PhantomData<(Sy, Cp)>,
}

impl<'src, B, Sy, Cp> BuilderState<'src> for GreenExtra<B, Sy, Cp>
where
    Sy: Copy,
    Self: Inspector<'src, Input<'src>, Checkpoint = Cp>,
    B: Builder<Sy, Cp, Checkpoint = Cp>,
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

    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>) {
        self.builder.token(kind, slice);
    }
}

impl<'src, B, Sy, Cp> Inspector<'src, Input<'src>> for GreenExtra<B, Sy, Cp>
where
    Cp: Copy,
    B: Builder<Sy, Cp, Checkpoint = Cp>,
{
    type Checkpoint = Cp;

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

pub trait Builder<Sy, Cp> {
    type Checkpoint;
    fn token(&mut self, kind: Sy, slice: Option<&str>);
    fn checkpoint(&self) -> Self::Checkpoint;
    fn revert_to(&mut self, checkpoint: Self::Checkpoint);
    fn start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Sy);
    fn finish_node(&mut self);
}

impl<'cache, 'interner, Sy, Cp> Builder<Sy, Cp> for GreenNodeBuilder<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    type Checkpoint = cstree::build::Checkpoint;

    fn token(&mut self, kind: Sy, slice: Option<&str>) {
        match slice {
            Some(slice) => self.token(kind, slice),
            None => self.static_token(kind),
        }
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        self.checkpoint()
    }

    fn revert_to(&mut self, checkpoint: Self::Checkpoint) {
        self.revert_to(checkpoint);
    }

    fn start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Sy) {
        self.start_node_at(checkpoint, kind);
    }

    fn finish_node(&mut self) {
        self.finish_node();
    }
}
