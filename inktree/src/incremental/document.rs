use chumsky::inspector::Inspector;
use cstree::{build::NodeCache, interning::MultiThreadedTokenInterner};
use std::fmt::Debug;

use crate::{
    engine::{Builder, parse_with_cache},
    language::Syntax,
};

pub struct DocumentSession<'interner, 'borrow> {
    cache: NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
}

impl<'interner, 'borrow> DocumentSession<'interner, 'borrow> {
    pub fn new(interner: &'borrow MultiThreadedTokenInterner) -> Self
where {
        let cache = NodeCache::from_interner(&*interner);

        Self { cache: cache }
    }

    pub fn parse<'src, Err, Sy>(
        cache: &mut NodeCache<'interner, &'borrow MultiThreadedTokenInterner>,
        input: &'src str,
    ) where
        Sy: Syntax,
        Err: chumsky::error::Error<'src, &'src str> + Debug,
        'interner: 'src,
    {
        let _ = parse_with_cache::<Sy::Root, Err, Sy>(cache, input).unwrap();
    }

    pub fn open<'src, Err, Sy>(
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
        let mut a: DocumentSession<'interner, 'borrow> = Self {
            cache: NodeCache::with_interner(interner),
        };

        Self::parse::<Err, Sy>(&mut a.cache, initial);

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
