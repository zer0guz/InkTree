mod kind;
mod node;
mod parseable;
mod syntax;
mod token;

use chumsky::{Parser, extra::ParserExtra};
pub use kind::*;
pub use node::*;
pub use parseable::Parseable;
pub use syntax::Syntax;
pub use token::*;

// --------------------- Language ---------------------

pub trait Language: Syntax + 'static {
    #[type_const]
    const ROOT: u32;
    #[type_const]
    const COUNT: usize;

    /// Enum variants indexed by discriminant
    const KINDS: [Self; Self::COUNT];

    /// Error token / error node kind.
    const ERROR: u32;

    /// Kinds treated as extras (whitespace, comments, …).
    const EXTRAS: &'static [u32];

    fn parser<'src, Extra>() -> impl Parser<'src, &'src str, (), Extra> + Clone
    where
        Extra: ParserExtra<'src, &'src str>,
    {
        KindMarker::<{ Self::ROOT }, Self>::parser()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::{Parser, error::EmptyErr, extra::Full};

    use crate::language::{
        node::{CardMany, CardOne, CardOneOrMore, CardZeroOrOne, Child, Cons, Nil, RoleToken},
        syntax::Syntax,
        token::TokenSpec,
    }; // or `use crate::Syntax;` if you re-export it

    #[repr(u32)]
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum MiniLang {
        Root,
        A,
        B,
    }

    impl cstree::Syntax for MiniLang {
        fn from_raw(_raw: cstree::RawSyntaxKind) -> Self {
            MiniLang::A
        }

        fn into_raw(self) -> cstree::RawSyntaxKind {
            cstree::RawSyntaxKind(self as u32)
        }

        fn static_text(self) -> Option<&'static str> {
            None
        }
    }

    impl Syntax for MiniLang {
        fn static_text(self) -> Option<&'static str> {
            match self {
                MiniLang::Root => None,
                MiniLang::A => Some("a"),
                MiniLang::B => Some("b"),
            }
        }
    }

    impl Language for MiniLang {
        const KINDS: [Self; 3] = [MiniLang::Root, MiniLang::A, MiniLang::B];
        #[type_const]
        const ROOT: u32 = MiniLang::Root as u32;
        #[type_const]
        const COUNT: usize = 3;
        const ERROR: u32 = MiniLang::Root as u32;
        const EXTRAS: &'static [u32] = &[];
    }

    // --- Token specs for A and B as static tokens ---

    impl TokenSpec<true> for KindMarker<{ MiniLang::A as u32 }, MiniLang> {
        type Parser = KindMarker<{ MiniLang::A as u32 }, MiniLang>;

        //const IDX:u32=MiniLang::A;
    }

    impl TokenSpec<true> for KindMarker<{ MiniLang::B as u32 }, MiniLang> {
        type Parser = KindMarker<{ MiniLang::B as u32 }, MiniLang>;
    }

    type L = MiniLang;

    // Root := "a" "b"
    type RootChildren =
        Cons<Child<1, CardOne, RoleToken>, Cons<Child<2, CardOne, RoleToken>, Nil<MiniLang>>>;

    // OptAB := "a"? "b"
    type OptABChildren = Cons<
        Child<{ MiniLang::A as u32 }, CardZeroOrOne, RoleToken>,
        Cons<Child<{ MiniLang::B as u32 }, CardOne, RoleToken>, Nil<L>>,
    >;

    // ManyAB := "a"* "b"
    type ManyABChildren = Cons<
        Child<{ MiniLang::A as u32 }, CardMany, RoleToken>,
        Cons<Child<{ MiniLang::B as u32 }, CardOne, RoleToken>, Nil<L>>,
    >;

    // PlusAB := "a"+ "b"
    type PlusABChildren = Cons<
        Child<{ MiniLang::A as u32 }, CardOneOrMore, RoleToken>,
        Cons<Child<{ MiniLang::B as u32 }, CardOne, RoleToken>, Nil<L>>,
    >;

    #[test]
    fn can_build_struct_parser_type() {
        let _parser = struct_parser::<RootChildren, _, Full<EmptyErr, (), ()>>();
    }

    #[test]
    fn root_node_consumes_two_chars() {
        let parser = struct_parser::<RootChildren, _, Full<EmptyErr, (), ()>>();

        let ok = parser.clone().parse("ab").into_result();
        assert!(ok.is_ok(), "expected \"ab\" to parse, got: {:?}", ok);

        let err = parser.clone().parse("a").into_result();
        assert!(err.is_err(), "expected \"a\" to fail, got: {:?}", err);
    }

    #[test]
    fn card_zero_or_one_behaves_like_optional() {
        let parser = struct_parser::<OptABChildren, _, Full<EmptyErr, (), ()>>();

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
        let parser = struct_parser::<ManyABChildren, _, Full<EmptyErr, (), ()>>();

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
        let parser = struct_parser::<PlusABChildren, _, Full<EmptyErr, (), ()>>();

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
