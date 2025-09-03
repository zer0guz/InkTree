use chumsky::{input::Cursor, inspector::Inspector};
use cstree::{
    build::{GreenNodeBuilder, NodeCache},
    green::GreenNode,
};

use crate::{
    chumsky_ext::{GreenState, Input},
    language::Syntax,
};

pub struct Builder<'cache, 'interner, Sy>
where
    Sy: Syntax,
    'interner: 'cache,
{
    pub builder: GreenNodeBuilder<'cache, 'interner, Sy>,
}

impl<'cache, 'interner, Sy> Builder<'cache, 'interner, Sy>
where
    Sy: Syntax,
{
    pub fn with_cache(cache: &'cache mut NodeCache<'interner>) -> Self {
        Builder::<'cache, 'interner> {
            builder: GreenNodeBuilder::<'cache, 'interner>::with_cache(cache),
        }
    }

    pub fn finish(self) -> (GreenNode, Option<NodeCache<'interner>>) {
        self.builder.finish()
    }
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
