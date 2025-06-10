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
    type Error: chumsky::error::Error<'src, &'src str>;
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
    type Error = <E as ParserExtra<'src, Input<'src>>>::Error;
    type Builder = E;

    fn revert_to(&mut self, checkpoint: Self::Checkpoint) {
        Self::_revert_to(self, checkpoint);
    }

    fn checkpoint(&self) -> Self::Checkpoint {
        Self::_checkpoint(self)
    }

    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>) {
        Self::_token(self, kind, slice);
    }
}
pub trait BuilderState<'src>: Inspector<'src, Input<'src>>
where
    Self::SyntaxKind: Copy,
{
    type SyntaxKind;

    fn _start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Self::SyntaxKind);
    fn _finish_node(&mut self);
    fn _revert_to(&mut self, checkpoint: Self::Checkpoint);
    fn _checkpoint(&self) -> <Self as Inspector<'src, Input<'src>>>::Checkpoint;
    fn _token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>);
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

    fn _start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Self::SyntaxKind) {
        self.builder._start_node_at(checkpoint, kind);
    }

    fn _finish_node(&mut self) {
        self.builder._finish_node();
    }

    fn _revert_to(&mut self, checkpoint: Self::Checkpoint) {
        self.builder._revert_to(checkpoint);
    }

    fn _checkpoint(&self) -> Self::Checkpoint {
        self.builder._checkpoint()
    }

    fn _token(&mut self, kind: Self::SyntaxKind, slice: Option<&'src str>) {
        self.builder._token(kind, slice);
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
        self.builder._checkpoint()
    }

    fn on_rewind<'parse>(
        &mut self,
        marker: &chumsky::input::Checkpoint<'src, 'parse, Input<'src>, Self::Checkpoint>,
    ) {
        self.builder._revert_to(*marker.inspector());
    }
}

pub trait Builder<Sy, Cp> {
    type Checkpoint;
    fn _token(&mut self, kind: Sy, slice: Option<&str>);
    fn _checkpoint(&self) -> Self::Checkpoint;
    fn _revert_to(&mut self, checkpoint: Self::Checkpoint);
    fn _start_node_at(&mut self, cp: Self::Checkpoint, kind: Sy);
    fn _finish_node(&mut self);
}

impl<'cache, 'interner, Sy, Cp> Builder<Sy, Cp> for GreenNodeBuilder<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    type Checkpoint = cstree::build::Checkpoint;

    fn _token(&mut self, kind: Sy, slice: Option<&str>) {
        match slice {
            Some(slice) => self.token(kind, slice),
            None => self.static_token(kind),
        }
    }

    fn _checkpoint(&self) -> Self::Checkpoint {
        self.checkpoint()
    }

    fn _revert_to(&mut self, checkpoint: Self::Checkpoint) {
        self.revert_to(checkpoint);
    }

    fn _start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Sy) {
        self.start_node_at(checkpoint, kind);
    }

    fn _finish_node(&mut self) {
        self.finish_node();
    }
}
