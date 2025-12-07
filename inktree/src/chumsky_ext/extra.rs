use chumsky::extra::Full;

use crate::engine::Builder;

pub type GreenExtra<'cache, 'interner, 'borrow, Err, Sy> =
    Full<Err, Builder<'cache, 'interner, 'borrow, Sy>, ()>;

pub trait GreenState<Sy> {
    type Checkpoint;

    fn start_node_at(&mut self, checkpoint: cstree::build::Checkpoint, kind: Sy);
    fn finish_node(&mut self);
    fn revert_to(&mut self, checkpoint: cstree::build::Checkpoint);
    fn checkpoint(&self) -> cstree::build::Checkpoint;
    fn token(&mut self, kind: Sy, slice: &str);
    fn static_token(&mut self, kind: Sy);
    fn start_node(&mut self, kind: Sy);
}
