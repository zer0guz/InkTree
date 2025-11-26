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
            type View<S: ::inktree::State> = AstNodeWrapper<$variant, S, $lang>;
        }
    };
    ($lang:ident::$variant:ident => $alias:ident, $ast_enum:ty) => {
        /// Typed AST node alias (with typestate parameter).
        pub type $alias<S> = $crate::AstNodeWrapper<$variant, S, $lang>;

        unsafe impl $crate::Kind for $variant {
            type Syntax = $lang;
            const KINDS: &'static [Self::Syntax] = &[$lang::$variant];
        }

        impl $crate::NodeKind for $variant {
            type View<S: ::inktree::State> = $ast_enum;
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
macro_rules! struct_accessor_sig_impl {
    (req_token, $field:ident, $ty:ident) => {
        (
            quote! {
                fn #$field(&self) -> S::Out<#$ty<S::ChildState>>;
            },
            quote! {
                #[inline]
                fn #$field(&self) -> S::Out<#$ty<S::ChildState>> {
                    ::inktree::lift_token(self)
                }
            },
        )
    };
    (req_node, $field:ident, $ty:ident) => {
        (
            quote! {
                fn #$field(&self) -> S::Out<#$ty<S::ChildState>>;
            },
            quote! {
                #[inline]
                fn #$field(&self) -> S::Out<#$ty<S::ChildState>> {
                    ::inktree::lift_child(self)
                }
            },
        )
    };
    (opt_token, $field:ident, $ty:ident) => {
        (
            quote! {
                fn #$field(&self) -> ::core::option::Option<#$ty<S>>;
            },
            quote! {
                #[inline]
                fn #$field(&self) -> ::core::option::Option<#$ty<S>> {
                    ::inktree::token(&self)
                }
            },
        )
    };
    (opt_node, $field:ident, $ty:ident) => {
        (
            quote! {
                fn #$field(&self) -> ::core::option::Option<#$ty<S>>;
            },
            quote! {
                #[inline]
                fn #$field(&self) -> ::core::option::Option<#$ty<S>> {
                    ::inktree::child(&self)
                }
            },
        )
    };
    (rep_token, $field:ident, $ty:ident) => {
        (
            quote! {
                fn #$field(&self) -> ::std::vec::Vec<#$ty<S>>;
            },
            quote! {
                #[inline]
                fn #$field(&self) -> ::std::vec::Vec<#$ty<S>> {
                    use ::inktree::AstToken;
                    self
                        .children_with_tokens()
                        .filter_map(|it| it.into_token())
                        .filter_map(|t| <$ty<S> as AstToken>::cast(&t))
                        .collect()
                }
            },
        )
    };
    (rep_node, $field:ident, $ty:ident) => {
        (
            quote! {
                fn #$field(&self) -> ::std::vec::Vec<#$ty<S>>;
            },
            quote! {
                #[inline]
                fn #$field(&self) -> ::std::vec::Vec<#$ty<S>> {
                    use ::inktree::AstNode;
                    self
                        .children()
                        .filter_map(<$ty<S> as AstNode>::cast)
                        .collect()
                }
            },
        )
    };
}
