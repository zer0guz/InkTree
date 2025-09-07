use std::fmt::Debug;
use cstree::{
    build::NodeCache,
    interning::MultiThreadedTokenInterner,
};

use crate::{chumsky_ext::Input, engine::parse_with_cache, language::Syntax};

pub struct DocumentSession<'src, 'interner, 'borrow> {
    input: &'src str,
    cache: Option<NodeCache<'interner, &'borrow MultiThreadedTokenInterner>>,
}

impl<'src, 'borrow, 'interner> DocumentSession<'src, 'interner, 'borrow> {
    pub fn new(input:&'src str,interner: &'borrow MultiThreadedTokenInterner) -> Self
where {
        let cache = NodeCache::from_interner(&*interner);

        Self {
            cache: Some(cache),
            input,
        }
    }

    pub fn parse<'parse, Err, Sy>(&'parse mut self)
    where
        Sy: Syntax,
        Err: chumsky::error::Error<'src, &'src str> + Debug + 'src,
        'borrow: 'src,
        'interner: 'src,
    {
        let cache = self.cache.take().unwrap();
        let _ = parse_with_cache::<Sy::Root, Err, Sy>(cache, self.input).unwrap();
    }

    pub fn open<Err, Sy>(
        initial: &'src str,
        interner: &'interner mut &'borrow MultiThreadedTokenInterner,
    ) -> Result<Self, Err>
    where
        Err: chumsky::error::Error<'src, Input<'src>> + 'src + Debug,
        Sy: Syntax,
        'borrow: 'src,
        'interner: 'src,
    {
        let mut a = Self {
            cache: Some(NodeCache::with_interner(interner)),
            input: initial,
        };

        a.parse::<Err, Sy>();

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
