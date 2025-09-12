use chumsky::{input::Cursor, inspector::Inspector};
use cstree::{
    build::{GreenNodeBuilder, NodeCache},
    green::GreenNode,
    interning::MultiThreadedTokenInterner,
};

use crate::{chumsky_ext::GreenState, language::Syntax};

pub struct Builder<'cache, 'interner, 'borrow, Sy>
where
    Sy: Syntax,
{
    pub builder: GreenNodeBuilder<'cache, 'interner, Sy, &'borrow MultiThreadedTokenInterner>,
}

impl<'src, 'cache, 'interner, 'borrow, Sy> Builder<'cache, 'interner, 'borrow, Sy>
where
    Sy: Syntax,
{
    pub fn with_cache(
        cache: &'cache mut NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
    ) -> Self {
        Builder::<'cache, 'interner, 'borrow> {
            builder: GreenNodeBuilder::with_cache(cache),
        }
    }

    pub fn finish(
        self,
    ) -> (
        GreenNode,
        Option<NodeCache<'interner, &'borrow MultiThreadedTokenInterner>>,
    ) {
        self.builder.finish()
    }
}

impl<'src, 'cache, 'interner, 'borrow, Sy> Inspector<'src, &'src str>
    for Builder<'cache, 'interner, 'borrow, Sy>
where
    Sy: Syntax,
{
    type Checkpoint = cstree::build::Checkpoint;

    fn on_token(&mut self, _: &char) {}

    fn on_save<'parse>(&self, _: &Cursor<'src, 'parse, &'src str>) -> Self::Checkpoint {
        self.builder.checkpoint()
    }

    fn on_rewind<'parse>(
        &mut self,
        marker: &chumsky::input::Checkpoint<'src, 'parse, &'src str, Self::Checkpoint>,
    ) {
        self.builder.revert_to(*marker.inspector());
    }
}

impl<'cache, 'interner, 'borrow, Sy> GreenState<Sy> for Builder<'cache, 'interner, 'borrow, Sy>
where
    Sy: Syntax,
{
    type Checkpoint = cstree::build::Checkpoint;

    fn start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Sy) {
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

    fn token(&mut self, kind: Sy, slice: &str) {
        self.builder.token(kind, slice);
    }

    fn static_token(&mut self, kind: Sy) {
        self.builder.static_token(kind);
    }
}
