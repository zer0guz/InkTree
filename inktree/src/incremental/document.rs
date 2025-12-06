use chumsky::inspector::Inspector;
use cstree::{build::NodeCache, interning::MultiThreadedTokenInterner};
use std::{cell::OnceCell, fmt::Debug, marker::PhantomData, mem::MaybeUninit, sync::OnceLock};

use crate::{
    AstNodeWrapper, SyntaxNode, Unchecked,
    engine::{Builder, ParserEngine},
    engine::Syntax,
};

pub struct DocumentSession<'interner, 'borrow, Sy: Syntax> {
    cache: NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
    root: OnceCell<SyntaxNode<Sy>>,
    _phantom: PhantomData<Sy>,
}

impl<'interner, 'borrow, Sy: Syntax> DocumentSession<'interner, 'borrow, Sy> {
    pub fn new(interner: &'borrow MultiThreadedTokenInterner) -> Self
where {
        let cache = NodeCache::from_interner(&*interner);

        Self {
            cache,
            _phantom: PhantomData,
            root: OnceCell::new(),
        }
    }

    fn root(&self) -> &SyntaxNode<Sy> {
        self.root.get().unwrap()
    }

    pub fn parse<'src, Err>(
        &mut self,
        input: &'src str,
    ) -> Result<AstNodeWrapper<Sy::Root, Unchecked, Sy>, ()>
    where
        Sy: Syntax,
        Err: chumsky::error::Error<'src, &'src str> + Debug,
        'interner: 'src,
    {
        let green = <Sy as ParserEngine>::parse_with_cache::<Err>(&mut self.cache, input).unwrap();
        let root = SyntaxNode::new_root(green);

        self.root.set(root);

        let ast = AstNodeWrapper::from(self.root().clone());
        Ok(ast)
    }

    pub fn open<'src, Err>(
        initial: &'src str,
        interner: &'interner mut &'borrow MultiThreadedTokenInterner,
    ) -> Result<Self, Err>
    where
        Err: chumsky::error::Error<'src, &'src str> + Debug,
        Builder<'src, 'interner, 'borrow, Sy>:
            Inspector<'src, &'src str, Checkpoint = cstree::build::Checkpoint>,
        Sy: Syntax,
        'interner: 'src,
    {
        let mut a: DocumentSession<'interner, 'borrow, Sy> = Self {
            cache: NodeCache::with_interner(interner),
            _phantom: PhantomData,
            root: OnceCell::new(),
        };

        Self::parse::<Err>(&mut a, initial);

        Ok(a)
    }

    // /// Apply a batch of LSP changes, re‐parsing only the minimal affected subtrees.
    // pub fn apply_changes<Err>(&mut self, changes: &[()]) -> Result<(), Err>
    // where
    //     Err: chumsky::error::Error<'src, &'src str> + 'static,
    // {
    //     for change in changes {
    //         // 1) LSP line/col → char offsets
    //         let start = self.buffer.line_to_char(change.range.start.line)
    //             + change.range.start.character as usize;
    //         let end = self.buffer.line_to_char(change.range.end.line)
    //             + change.range.end.character as usize;
    //         let insert = change.text.clone();

    //         // 2) Edit the rope: remove then insert :contentReference[oaicite:0]{index=0}
    //         self.buffer.remove(start..end);
    //         if !insert.is_empty() {
    //             self.buffer.insert(start, &insert);
    //         }

    //         // 3) Build a TextRange of the “dirty” chars
    //         let dirty = TextRange::new(
    //             TextSize::from(start as u32),
    //             TextSize::from((start + insert.chars().count()) as u32),
    //         ); // :contentReference[oaicite:1]{index=1}

    //         // 4) Find the smallest red‐tree element covering that span
    //         let elem = self.root.covering_element(dirty); // :contentReference[oaicite:2]{index=2}
    //         // 5) Turn it into a `SyntaxNode<Sy>`:
    //         let node: SyntaxNode<Sy> = match elem {
    //             ResolvedElementRef::Node(n) => n.syntax().clone(), // :contentReference[oaicite:3]{index=3}
    //             ResolvedElementRef::Token(t) => t.syntax().parent().clone(),
    //         };

    //         // 6) Slice out the new text from the rope
    //         let range = node.text_range();
    //         let slice = self
    //             .buffer
    //             .slice(range.start().to_usize()..range.end().to_usize());
    //         let text = slice.to_string();

    //         // 7) Re‐parse just that variant
    //         let green_sub = if node.kind() == LangSyntax::into_raw(Sy::ROOT) {
    //             self.engine.parse_full::<Err>(&text)?
    //         } else {
    //             // you’ll generate a `match` on each nonterminal kind here
    //             self.engine.parse_node::<S, Err>(&text)?
    //         };

    //         // 8) Splice it back into the old tree
    //         let new_green = node.replace_with(green_sub);
    //         self.root = SyntaxNode::new_root(new_green.clone());
    //     }
    //     Ok(())
    // }

    // /// Access the current CST for hover/completion/etc.
    // pub fn syntax(&self) -> &SyntaxNode<Sy> {
    //     &self.root
    // }
}
