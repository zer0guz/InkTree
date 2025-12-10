mod parseable;
mod syntax;
mod kind;
mod node;

pub use parseable::*;
pub use syntax::*;
pub use kind::*;

// --------------------- Language ---------------------

pub trait Language<const COUNT:usize>: Syntax + 'static {    
    /// Total number of kinds. Used to size const tables.
    const KINDS: [Self;COUNT];

    /// Root node kind.
    const ROOT: Self;

    /// Error token / error node kind.
    const ERROR: Self;

    /// Kinds treated as extras (whitespace, comments, …).
    const EXTRAS: &'static [Self];

    // Per-kind metadata. Length MUST be KIND_COUNT.
    //const KINDS: &'static [KindInfo<Self>];
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::{Parser, error::EmptyErr, extra::Full};

    use crate::language::{node::{BuildStructSeq, CardMany, CardOne, CardOneOrMore, CardZeroOrOne, Child, ChildList, Cons, Nil, RoleToken}, syntax::Syntax}; // or `use crate::Syntax;` if you re-export it

    #[repr(u16)]
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum MiniLang {
        Root,
        A,
        B,
    }

    // cstree integration, unchanged in spirit
    impl cstree::Syntax for MiniLang {
        fn from_raw(raw: cstree::RawSyntaxKind) -> Self {
            unsafe { core::mem::transmute::<u16, MiniLang>(raw.0 as u16) }
        }

        fn into_raw(self) -> cstree::RawSyntaxKind {
            cstree::RawSyntaxKind(self as u16 as u32)
        }

        fn static_text(self) -> Option<&'static str> {
            None
        }
    }

    // Our own Syntax: used by the static-token Parseable impl.
    impl Syntax for MiniLang {
        fn static_text(self) -> Option<&'static str> {
            match self {
                MiniLang::Root => None,
                MiniLang::A => Some("a"),
                MiniLang::B => Some("b"),
            }
        }
    }

    const N: usize = 3;

    impl Language<N> for MiniLang {
        const KINDS: [Self; N] = [MiniLang::Root, MiniLang::A, MiniLang::B];

        const ROOT: Self = MiniLang::Root;
        const ERROR: Self = MiniLang::Root;
        const EXTRAS: &'static [Self] = &[];
    }

    // --- Token specs for A and B as static tokens ---

    impl TokenSpec<{ MiniLang::A as u16 }, N, true>
        for KindMarker<{ MiniLang::A as u16 }, N, MiniLang>
    {
        type Parser = KindMarker<{ MiniLang::A as u16 }, N, MiniLang>;
    }

    impl TokenSpec<{ MiniLang::B as u16 }, N, true>
        for KindMarker<{ MiniLang::B as u16 }, N, MiniLang>
    {
        type Parser = KindMarker<{ MiniLang::B as u16 }, N, MiniLang>;
    }

    type L = MiniLang;

    // Root := "a" "b"
    type RootChildren = Cons<
        Child<{ MiniLang::A as u16 }, CardOne, RoleToken>,
        Cons<Child<{ MiniLang::B as u16 }, CardOne, RoleToken>, Nil<N, L>>,
    >;

    // OptAB := "a"? "b"
    type OptABChildren = Cons<
        Child<{ MiniLang::A as u16 }, CardZeroOrOne, RoleToken>,
        Cons<Child<{ MiniLang::B as u16 }, CardOne, RoleToken>, Nil<N, L>>,
    >;

    // ManyAB := "a"* "b"
    type ManyABChildren = Cons<
        Child<{ MiniLang::A as u16 }, CardMany, RoleToken>,
        Cons<Child<{ MiniLang::B as u16 }, CardOne, RoleToken>, Nil<N, L>>,
    >;

    // PlusAB := "a"+ "b"
    type PlusABChildren = Cons<
        Child<{ MiniLang::A as u16 }, CardOneOrMore, RoleToken>,
        Cons<Child<{ MiniLang::B as u16 }, CardOne, RoleToken>, Nil<N, L>>,
    >;

    // Generic “struct node” parser for a given child list.
    fn struct_parser<List>() -> impl Parser<'static, &'static str, (), Full<EmptyErr,(),()>> + Clone
    where
        L: Language<N>,
        List: ChildList<N, L>,
        (): BuildStructSeq<N, L, List>,
    {
        <() as BuildStructSeq<N, L, List>>::build::<'static, Full<EmptyErr,(),()>>()
    }

    #[test]
    fn can_build_struct_parser_type() {
        let _parser = struct_parser::<RootChildren>();
    }

    #[test]
    fn root_node_consumes_two_chars() {
        let parser = struct_parser::<RootChildren>();

        let ok = parser.clone().parse("ab").into_result();
        assert!(ok.is_ok(), "expected \"ab\" to parse, got: {:?}", ok);

        let err = parser.clone().parse("a").into_result();
        assert!(err.is_err(), "expected \"a\" to fail, got: {:?}", err);
    }

    #[test]
    fn card_zero_or_one_behaves_like_optional() {
        let parser = struct_parser::<OptABChildren>();

        assert!(
            parser.clone().parse("b").into_result().is_ok(),
            "A? B should accept \"b\""
        );
        assert!(
            parser.clone().parse("ab").into_result().is_ok(),
            "A? B should accept \"ab\""
        );

        assert!(
            parser.clone().parse("").into_result().is_err(),
            "A? B should reject empty input"
        );
        assert!(
            parser.clone().parse("a").into_result().is_err(),
            "A? B should reject \"a\" (missing B)"
        );
        assert!(
            parser.clone().parse("abb").into_result().is_err(),
            "A? B should reject extra input"
        );
    }

    #[test]
    fn card_many_allows_zero_or_more_before_b() {
        let parser = struct_parser::<ManyABChildren>();

        assert!(
            parser.clone().parse("b").into_result().is_ok(),
            "A* B should accept \"b\""
        );
        assert!(
            parser.clone().parse("ab").into_result().is_ok(),
            "A* B should accept \"ab\""
        );
        assert!(
            parser.clone().parse("aaab").into_result().is_ok(),
            "A* B should accept \"aaab\""
        );

        assert!(
            parser.clone().parse("").into_result().is_err(),
            "A* B should reject empty input"
        );
        assert!(
            parser.clone().parse("a").into_result().is_err(),
            "A* B should reject \"a\" (missing B)"
        );
    }

    #[test]
    fn card_one_or_more_requires_at_least_one_before_b() {
        let parser = struct_parser::<PlusABChildren>();

        assert!(
            parser.clone().parse("ab").into_result().is_ok(),
            "A+ B should accept \"ab\""
        );
        assert!(
            parser.clone().parse("aab").into_result().is_ok(),
            "A+ B should accept \"aab\""
        );

        assert!(
            parser.clone().parse("").into_result().is_err(),
            "A+ B should reject empty"
        );
        assert!(
            parser.clone().parse("a").into_result().is_err(),
            "A+ B should reject \"a\" (missing B)"
        );
        assert!(
            parser.clone().parse("b").into_result().is_err(),
            "A+ B should reject \"b\" (missing A)"
        );
    }
}
