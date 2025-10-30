#[macro_export]
macro_rules! ast_token {
    // without sink
    ($lang_name:ident :: $name:ident, $ast_name:ident) => {
        struct $ast_name {
            syntax: $crate::cstree::syntax::SyntaxToken<$lang_name>,
        }

        impl $crate::derive::AstToken for $ast_name {
            type Syntax = $lang_name;
            const KINDS: &[Self::Syntax] = &[$lang_name::$name];

            fn syntax(&self) -> &$crate::cstree::syntax::SyntaxToken<$lang_name> {
                &self.syntax
            }

            fn cast(syntax: $crate::cstree::syntax::SyntaxToken<$lang_name>) -> Option<Self>
            where
                Self: Sized,
            {
                if syntax.kind() == $lang_name::$name {
                    Some($ast_name { syntax })
                } else {
                    None
                }
            }

            fn text(&self) -> &str {
                self.syntax.resolve_text(__inktree_support::resolver())
            }
        }
    };
}
#[macro_export]
macro_rules! ast_node {
    // without sink
    ($lang_name:ident :: $name:ident, $ast_name:ident) => {
        struct $ast_name {
            syntax: $crate::cstree::syntax::SyntaxNode<$lang_name>,
        }

        impl $crate::derive::AstNode for $ast_name {
            type Syntax = $lang_name;
            const KINDS: &[Self::Syntax] = &[$lang_name::$name];

            fn syntax(&self) -> &$crate::cstree::syntax::SyntaxNode<$lang_name> {
                &self.syntax
            }

            fn cast(syntax: $crate::cstree::syntax::SyntaxNode<$lang_name>) -> Option<Self>
            where
                Self: Sized,
            {
                if syntax.kind() == $lang_name::$name {
                    Some($ast_name { syntax })
                } else {
                    None
                }
            }
        }
    };
}
