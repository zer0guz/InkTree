#[macro_export]
macro_rules! ast_node_kind {
    // Simple form with alias name explicitly given
    ($lang:ident::$variant:ident => $alias:ident) => {
        /// Typed AST node alias (with typestate parameter).
        pub type $alias<S> = $crate::AstNodeWrapper<$variant, S, $lang>;

        unsafe impl $crate::Kind for $variant {
            type Syntax = $lang;
            type Wrapper<S> = $alias<S>;
            const KINDS: &'static [Self::Syntax] = &[$lang::$variant];
        }

        impl $crate::NodeKind for $variant {
            type View<S: ::inktree::State> = ::inktree::AstNodeWrapper<$variant, S, $lang>;
        }
    };
    ($lang:ident::$variant:ident => $alias:ident, $ast_enum:ident) => {
        /// Typed AST node alias (with typestate parameter).
        pub type $alias<S> = $crate::AstNodeWrapper<$variant, S, $lang>;

        unsafe impl $crate::Kind for $variant {
            type Syntax = $lang;
            type Wrapper<S> = $alias<S>;

            const KINDS: &'static [Self::Syntax] = &[$lang::$variant];
        }

        impl $crate::NodeKind for $variant {
            type View<S: ::inktree::State> = $ast_enum<S>;
        }
    };
}

#[macro_export]
macro_rules! ast_token_kind {
    ($lang:ident::$variant:ident => $alias:ident) => {
        /// Typed AST token alias (with typestate parameter).
        pub type $alias<S> = $crate::AstTokenWrapper<$variant, S, $lang>;

        unsafe impl $crate::Kind for $variant {
            type Syntax = $lang;
            type Wrapper<S> = $alias<S>;
            const KINDS: &'static [Self::Syntax] = &[$lang::$variant];
        }

        impl $crate::TokenKind for $variant {}
    };
}
#[macro_export]
macro_rules! ast_node_anon_enum {
    // Public entry
    ($lang:ident::$name:ident( $( $variant:ident : $vkind:ident )|+ $(|)? ) => $alias:ident,$enum_name:ident) => {
        pub struct $name;

        // enum ExprAtomAstEnum<S> { Number(NumberAst<S>), Expr(ExprAst<S>), ... }
        pub enum $enum_name<S: $crate::State> {
            $( $variant(<$variant as $crate::Kind>::Wrapper<S>), )*
        }

        // type ExprAtomAst<S> = AstNodeWrapper<TestMarker, S, TestLang>;
        pub type $alias<S> = $crate::AstNodeWrapper<$name, S, $lang>;

        unsafe impl $crate::Kind for $name {
            type Syntax = $lang;
            type Wrapper<S> = $alias<S>;
            const KINDS: &'static [Self::Syntax] = &[
                $( $lang::$variant, )*
            ];
        }

        impl $crate::NodeKind for $name {
            type View<S: $crate::State> = $enum_name<S>;
        }

        impl<S: $crate::State> $crate::View for $enum_name<S> {
            type Kind = $name;
            type State = S;

            fn from_raw(
                raw: $crate::cstree::prelude::SyntaxElement<
                    <Self::Kind as $crate::Kind>::Syntax,
                    $crate::ParseError,
                >,
            ) -> Self {
                match raw.kind() {
                    $(
                        $lang::$variant => $crate::ast_node_anon_enum!(@from_raw_variant
                            $enum_name, $variant, $vkind, raw
                        ),
                    )*
                    _ => unreachable!("anon enum element kind not in KINDS"),
                }
            }
        }
    };

    // Helper arm for Token variants
    (@from_raw_variant $enum_name:ident, $variant:ident, Token, $raw:ident) => {
        $enum_name::$variant(
            $raw.as_token()
                .expect("anon enum variant is declared as Token but got Node")
                .clone()
                .into()
        )
    };

    // Helper arm for Node variants
    (@from_raw_variant $enum_name:ident, $variant:ident, Node, $raw:ident) => {
        $enum_name::$variant(
            $raw.as_node()
                .expect("anon enum variant is declared as Node but got Token")
                .clone()
                .into()
        )
    };
}
