use chumsky::{extra::Full, inspector::Inspector};

use crate::engine::Builder;


pub type Input<'src> = &'src str;
pub type GreenExtra<'cache, 'interner, Err, Sy> = Full<Err, Builder<'cache, 'interner, Sy>, ()>;

pub trait GreenState<'src>: Inspector<'src, Input<'src>> {
    type SyntaxKind;

    fn start_node_at(&mut self, checkpoint: Self::Checkpoint, kind: Self::SyntaxKind);
    fn finish_node(&mut self);
    fn revert_to(&mut self, checkpoint: Self::Checkpoint);
    fn checkpoint(&self) -> <Self as Inspector<'src, Input<'src>>>::Checkpoint;
    fn token(&mut self, kind: Self::SyntaxKind, slice: &'src str);
    fn static_token(&mut self, kind: Self::SyntaxKind);
}
