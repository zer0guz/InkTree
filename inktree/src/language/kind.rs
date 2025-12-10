use std::marker::PhantomData;

use chumsky::error::Error;
use chumsky::prelude::todo;
use chumsky::{Parser, extra::ParserExtra};

use crate::{Language, chumsky_ext::BuilderParser};

/// Shorthand to access the language associated with a kind specification.
pub type LangAlias<Spec, Tag> = <Spec as KindSpec<Tag>>::Lang;

/// Marker type for a concrete kind `IDX` of language `L` with `N` total kinds.
///
/// Each `KindMarker` acts as the central point where all behaviour for the
/// corresponding syntax kind is attached via traits.
pub struct KindMarker<const IDX: u32, L>(pub PhantomData<L>);

impl<const IDX: u32, L> KindMarker<IDX, L> {
    pub fn parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        todo()
    }
}

/// Common information for all kinds in a language at a given index.
///
/// Implemented automatically for `KindMarker` when the corresponding
/// per-kind spec traits are implemented.
pub trait KindSpec<Tag> {
    /// Language that this kind belongs to.
    type Lang: Language;
    #[type_const]
    const IDX: u32;

    /// Tag identifying the role of this kind (token, node, etc.).
    type Tag;
}

/// Behaviour for a kind under a specific tag.
///
/// This is the type-level entry point to obtain a parser based on the kind's
/// role (token, node, etc.).
pub trait KindBehaviorByTag<Tag, L: Language> {
    /// Build the base parser for this kind under the given tag.
    fn base_parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>;
}

/// Builder-level parser for a given kind, specialized by tag.
///
/// This is the first layer that fixes `Extra = GreenExtra<..., L>`
/// and turns the pure grammar parser into a CST-building parser.
pub trait KindBuilderByTag<Tag, L: Language> {
    fn builder_parser<'src, 'cache, 'interner, 'borrow, 'extra, Err>()
    -> impl BuilderParser<'src, 'cache, 'interner, 'borrow, (), Err, L> + Clone + 'extra
    where
        Err: Error<'src, &'src str> + 'extra,
        'interner: 'cache,
        'borrow: 'interner,
        'cache: 'extra,
        'src: 'extra;
}
