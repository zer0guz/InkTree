use chumsky::{input::{Checkpoint, Cursor}, inspector::Inspector};
use cstree::{build::GreenNodeBuilder, Syntax};

use crate::chumksy::Input;


pub trait BuilderState<'src>: Inspector<'src, Input<'src>> {
    type SyntaxKind;

    fn start_node_at(
        &mut self,
        cp: <Self as Inspector<'src, Input<'src>>>::Checkpoint,
        kind: Self::SyntaxKind,
    );
    fn finish_node(&mut self);
    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&str>);
    fn revert_to(&mut self, cp: Self::Checkpoint);
    fn checkpoint(&self) -> <Self as Inspector<'src, Input<'src>>>::Checkpoint;
}

impl<'src, 'cache, 'interner, Sy> BuilderState<'src>
    for InspectorBuilderState<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    type SyntaxKind = Sy;

    fn start_node_at(
        &mut self,
        checkpoint: <Self as Inspector<'src, Input<'src>>>::Checkpoint,
        kind: Self::SyntaxKind,
    ) {
        self.builder.start_node_at(checkpoint, kind);
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn revert_to(&mut self, checkpoint: Self::Checkpoint) {
        self.builder.revert_to(checkpoint)
    }
    fn token(&mut self, kind: Self::SyntaxKind, slice: Option<&str>) {
        match slice {
            Some(slice) => self.builder.token(kind, slice),
            None => self.builder.static_token(kind),
        }
    }

    fn checkpoint(&self) -> <Self as Inspector<'src, Input<'src>>>::Checkpoint {
        self.builder.checkpoint()
    }
}

pub struct InspectorBuilderState<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    builder: GreenNodeBuilder<'cache, 'interner, Sy>,
}

impl<'src, 'cache, 'interner, Sy> Inspector<'src, Input<'src>>
    for InspectorBuilderState<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    type Checkpoint = cstree::build::Checkpoint;

    fn on_token(&mut self, _: &char) {}

    fn on_save<'parse>(&self, _: &Cursor<'src, 'parse, Input<'src>>) -> Self::Checkpoint {
        <Self as BuilderState>::checkpoint(self)
    }

    fn on_rewind<'parse>(
        &mut self,
        marker: &Checkpoint<'src, 'parse, Input<'src>, Self::Checkpoint>,
    ) {
        Self::revert_to(self, *marker.inspector());
    }
}

impl<'cache, 'interner, Sy> InspectorBuilderState<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    fn new(builder: GreenNodeBuilder<'cache, 'interner, Sy>) -> Self {
        Self { builder }
    }
    pub fn from(builder: GreenNodeBuilder<'cache, 'interner, Sy>) -> Self {
        Self::new(builder)
    }
}