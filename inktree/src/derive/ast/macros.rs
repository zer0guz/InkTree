#[macro_export]
macro_rules! ast_node_kind {
    // Simple form with alias name explicitly given
    ($lang:ident::$variant:ident => $alias:ident) => {
        /// Typed AST node alias (with typestate parameter).
        pub type $alias<S> = $crate::AstNodeWrapper<$variant, S, $lang>;

        unsafe impl $crate::Kind for $variant {
            type Syntax = $lang;
            const KINDS: &'static [Self::Syntax] = &[$lang::$variant];
        }

        impl $crate::NodeKind for $variant {
            type View<S: ::inktree::State> = ::inktree::AstNodeWrapper<$variant, S, $lang>;
        }
    };
    ($lang:ident::$variant:ident => $alias:ident, $ast_enum:ident) => {
        /// Typed AST node alias (with typestate parameter).
        pub type $alias<S: ::inktree::State> = $crate::AstNodeWrapper<$variant, S, $lang>;

        unsafe impl $crate::Kind for $variant {
            type Syntax = $lang;

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
            const KINDS: &'static [Self::Syntax] = &[$lang::$variant];
        }

        impl $crate::TokenKind for $variant {}
    };
}
#[macro_export]
macro_rules! enum_view_impl {
    // Public entry:
    //
    //   enum_view_impl!(TestLang::ExprAtomMarker(
    //       Number:Token | Ident:Token | Expr:Node |
    //   ) for ExprAtomAstEnum);
    //
    ($lang:ident::$kind:ident( $( $variant:ident : $vkind:ident )|+ $(|)? ) for $enum_name:ident) => {
        impl<S: $crate::State> $crate::View for $enum_name<S> {
            type Kind = $kind;
            type State = S;

            fn from_raw_token(
                raw: $crate::SyntaxToken<
                    <Self::Kind as $crate::Kind>::Syntax
                >,
            ) -> Self {
                match raw.kind() {
                    $(
                        $lang::$variant => $crate::enum_view_impl!(@from_raw_token_variant
                            $enum_name, $variant, $vkind, raw
                        ),
                    )*
                    _ => unreachable!("anon enum element kind not in KINDS (token)"),
                }
            }

            fn from_raw_node(
                raw: $crate::SyntaxNode<
                    <Self::Kind as $crate::Kind>::Syntax
                >,
            ) -> Self {
                match raw.kind() {
                    $(
                        $lang::$variant => $crate::enum_view_impl!(@from_raw_node_variant
                            $enum_name, $variant, $vkind, raw
                        ),
                    )*
                    _ => unreachable!("anon enum element kind not in KINDS (node)"),
                }
            }
        }
    };

    // ---------- helpers for from_raw_token ----------

    // Token variants are allowed in from_raw_token
    (@from_raw_token_variant $enum_name:ident, $variant:ident, Token, $raw:ident) => {
        $enum_name::$variant($raw.clone().into())
    };

    // Node variants should never go through from_raw_token
    (@from_raw_token_variant $enum_name:ident, $variant:ident, Node, $raw:ident) => {
        unreachable!(
            concat!(
                "anon enum variant `",
                stringify!($variant),
                "` is declared as Node but from_raw_token was called"
            )
        )
    };

    // ---------- helpers for from_raw_node ----------

    // Node variants are allowed in from_raw_node
    (@from_raw_node_variant $enum_name:ident, $variant:ident, Node, $raw:ident) => {
        $enum_name::$variant($raw.clone().into())
    };

    // Token variants should never go through from_raw_node
    (@from_raw_node_variant $enum_name:ident, $variant:ident, Token, $raw:ident) => {
        unreachable!(
            concat!(
                "anon enum variant `",
                stringify!($variant),
                "` is declared as Token but from_raw_node was called"
            )
        )
    };
}

#[macro_export]
macro_rules! ast_node_anon_enum {
    // Public entry:
    //
    //   ast_node_anon_enum!(TestLang::ExprAtomMarker(
    //       Number:Token | Ident:Token | Expr:Node |
    //   ) => ExprAtomAst, ExprAtomAstEnum);
    //
    ($lang:ident::$name:ident( $( $variant:ident : $vkind:ident )|+ $(|)? ) => $alias:ident,$enum_name:ident) => {
        #[derive(Debug)]
        pub struct $name;

        // enum ExprAtomAstEnum<S> {
        //   Number(AstTokenWrapper<Number, S, TestLang>),
        //   Ident(AstTokenWrapper<Ident, S, TestLang>),
        //   Expr(AstNodeWrapper<Expr, S, TestLang>),
        // }
        pub enum $enum_name<S: $crate::State> {
            $(
                $variant(
                    $crate::ast_node_anon_enum!(@variant_ty $lang, $variant, $vkind, S)
                ),
            )*
        }

        // type ExprAtomAst<S> = AstNodeWrapper<ExprAtomMarker, S, TestLang>;
        pub type $alias<S> = $crate::AstNodeWrapper<$name, S, $lang>;

        unsafe impl $crate::Kind for $name {
            type Syntax = $lang;
            const KINDS: &'static [Self::Syntax] = &[
                $( $lang::$variant, )*
            ];
        }

        impl $crate::NodeKind for $name {
            type View<S: $crate::State> = $enum_name<S>;
        }

        // Reuse the generic View impl
        $crate::enum_view_impl!($lang::$name( $( $variant : $vkind )|* ) for $enum_name);
    };

    // ---------- type selection for enum fields ----------

    // Token variant → AstTokenWrapper<Variant, S, Lang>
    (@variant_ty $lang:ident, $variant:ident, Token, $S:ident) => {
        $crate::AstTokenWrapper<$variant, $S, $lang>
    };

    // Node variant → AstNodeWrapper<Variant, S, Lang>
    (@variant_ty $lang:ident, $variant:ident, Node, $S:ident) => {
        $crate::AstNodeWrapper<$variant, $S, $lang>
    };
}
