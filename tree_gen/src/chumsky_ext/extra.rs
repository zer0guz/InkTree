use chumsky::{extra::Full, inspector::Inspector};

use crate::engine::Builder;

pub type Input<'src> = &'src str;
pub type GreenExtra<'cache, 'interner,'borrow, Err, Sy> = Full<Err, Builder<'cache, 'interner,'borrow, Sy>, ()>;

pub trait GreenState<'src,Sy>: Inspector<'src, Input<'src>> {

    fn start_node_at(&mut self, checkpoint: cstree::build::Checkpoint, kind: Sy);
    fn finish_node(&mut self);
    fn revert_to(&mut self, checkpoint: cstree::build::Checkpoint);
    fn checkpoint(&self) -> cstree::build::Checkpoint;
    fn token(&mut self, kind: Sy, slice: &'src str);
    fn static_token(&mut self, kind: Sy);
}
