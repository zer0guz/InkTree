use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::derive::parser::DslExpr;

pub enum AstShape {
    Token, // leaf token

    Node {
        fields: Vec<Field>,
    },

    Enum {
        variants: Vec<Ident>, // names of node/token types
    },
    Pratt {
        atom: Ident,
        prefix_ops: Vec<Ident>,
        infix_ops: Vec<Ident>,
    },
}

pub struct Field {
    pub name: Ident, // accessor name in the AST
    pub kind: Ident, // the node/token type name
    pub cardinality: Cardinality,
}

#[derive(PartialEq, Debug)]
pub enum Cardinality {
    One,
    Optional,
    Repeated,
}

pub struct AstGenCtx {
    counter: std::cell::Cell<u32>,
}

impl AstGenCtx {
    pub fn new() -> Self {
        Self { counter: std::cell::Cell::new(0) }
    }

    pub fn fresh_name(&self, parent: &Ident, kind: &str) -> Ident {
        let idx = self.counter.get();
        self.counter.set(idx + 1);
        Ident::new(&format!("{}_{}_{}", parent, kind, idx), parent.span())
    }
}
impl DslExpr {
    pub fn ast_shape(
        &self,
        rule_name: &Ident,
        ctx: &mut AstGenCtx,
        ast_shapes: &mut HashMap<Ident, AstShape>,
    ) -> AstShape {
        match self {
            DslExpr::Just(ident) => AstShape::Node {
                fields: vec![Field {
                    name: ident.clone(),
                    kind: ident.clone(),
                    cardinality: Cardinality::One,
                }],
            },

            DslExpr::Seq(exprs) => {
                let fields = exprs
                    .iter()
                    .map(|e| match e {
                        DslExpr::Opt(inner) => {
                            field_from_expr_with_card(inner, rule_name, ctx, ast_shapes, Cardinality::Optional)
                        }
                        DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                            field_from_expr_with_card(inner, rule_name, ctx, ast_shapes, Cardinality::Repeated)
                        }
                        _ => field_from_expr_with_card(e, rule_name, ctx, ast_shapes, Cardinality::One),
                    })
                    .collect();
                AstShape::Node { fields }
            }

            DslExpr::Opt(inner) => {
                let field = field_from_expr_with_card(inner, rule_name, ctx, ast_shapes, Cardinality::Optional);
                AstShape::Node { fields: vec![field] }
            }

            DslExpr::Star(inner) | DslExpr::Plus(inner) => {
                let field = field_from_expr_with_card(inner, rule_name, ctx, ast_shapes, Cardinality::Repeated);
                AstShape::Node { fields: vec![field] }
            }

            DslExpr::Call { name, .. } => AstShape::Node {
                fields: vec![Field {
                    name: name.clone(),
                    kind: name.clone(),
                    cardinality: Cardinality::One,
                }],
            },

            DslExpr::Alt(exprs) => {
                let mut variants = Vec::new();

                for e in exprs {
                    match e {
                        DslExpr::Just(ident) => variants.push(ident.clone()),
                        DslExpr::Call { name, .. } => variants.push(name.clone()),
                        _ => {
                            // generate helper for nested alt/seq/etc
                            let anon = ctx.fresh_name(rule_name, "Alt");
                            let helper_shape = e.ast_shape(&anon, ctx, ast_shapes);
                            ast_shapes.insert(anon.clone(), helper_shape);
                            variants.push(anon);
                        }
                    }
                }

                AstShape::Enum { variants }
            }
        }
    }
}

fn field_from_expr_with_card(
    expr: &DslExpr,
    parent: &Ident,
    ctx: &mut AstGenCtx,
    ast_shapes: &mut HashMap<Ident, AstShape>,
    card: Cardinality,
) -> Field {
    match expr {
        DslExpr::Just(ident) => Field { name: ident.clone(), kind: ident.clone(), cardinality: card },
        DslExpr::Call { name, .. } => Field { name: name.clone(), kind: name.clone(), cardinality: card },
        _ => {
            let anon = ctx.fresh_name(parent, "Seq");
            let helper = expr.ast_shape(&anon, ctx, ast_shapes);
            ast_shapes.insert(anon.clone(), helper);
            Field { name: anon.clone(), kind: anon, cardinality: card }
        }
    }
}

impl AstShape {
    pub fn codegen(&self, lang_ident: &Ident, name: &Ident) -> TokenStream {
        let ast_name = format_ident!("{}Ast", name);

        match self {
            AstShape::Token => {
                quote! {
                    #[derive(Clone)]
                    pub struct #ast_name(SyntaxToken<#lang_ident>);
                    impl AstToken for #ast_name {
                        type Syntax = #lang_ident;
                        const KINDS: &[Self::Syntax] = &[#lang_ident::#name];

                        fn cast(syntax: &SyntaxToken<Self::Syntax>) -> Option<Self> {
                            Some(Self(syntax.clone()))
                        }
                        fn syntax(&self) -> &SyntaxToken<Self::Syntax> { &self.0 }
                        fn text(&self) -> &str { self.0.resolve_text(__inktree_support::resolver()) }
                    }
                }
            }

            AstShape::Node { fields } => {
                let field_methods: Vec<_> = fields.iter().map(|field| {
                    let field_type = format_ident!("{}Ast", field.kind);
                    let field_name = syn::Ident::new(
                        &field.name.to_string().to_lowercase(),
                        field.name.span(),
                    );

                    match field.cardinality {
                        Cardinality::One => quote! {
                            pub fn #field_name(&self) -> #field_type {
                                self.syntax().children().find_map(#field_type::cast)
                                    .expect("missing required field")
                            }
                        },
                        Cardinality::Optional => quote! {
                            pub fn #field_name(&self) -> Option<#field_type> {
                                self.syntax().children().find_map(#field_type::cast)
                            }
                        },
                        Cardinality::Repeated => quote! {
                            pub fn #field_name(&self) -> impl Iterator<Item=#field_type> + '_ {
                                self.syntax().children().filter_map(#field_type::cast)
                            }
                        },
                    }
                }).collect();

                quote! {
                    #[derive(Clone)]
                    pub struct #ast_name(SyntaxNode<#lang_ident>);
                    impl AstNode for #ast_name {
                        type Syntax = #lang_ident;
                        const KINDS: &[Self::Syntax] = &[#lang_ident::#name];

                        fn cast(syntax: &SyntaxNode<Self::Syntax>) -> Option<Self> {
                            Some(Self(syntax.clone()))
                        }
                        fn syntax(&self) -> &SyntaxNode<Self::Syntax> { &self.0 }
                    }
                    impl #ast_name {
                        #(#field_methods)*
                    }
                }
            }

            AstShape::Enum { variants } => {
                let variant_defs: Vec<_> = variants.iter().map(|variant| {
                    let variant_ast = format_ident!("{}Ast", variant);
                    quote! { #variant(#variant_ast) }
                }).collect();

                let kind_consts: Vec<_> = variants.iter().map(|v| {
                    quote! { #lang_ident::#v }
                }).collect();

                let cast_arms: Vec<_> = variants.iter().map(|variant| {
                    let variant_ast = format_ident!("{}Ast", variant);
                    quote! {
                        if let Some(x) = #variant_ast::cast(syntax.clone()) {
                            return Some(#ast_name::#variant(x));
                        }
                    }
                }).collect();

                let syntax_arms: Vec<_> = variants.iter().map(|v| {
                    quote! { #ast_name::#v(inner) => inner.syntax() }
                }).collect();

                quote! {
                    #[derive(Clone)]
                    pub enum #ast_name {
                        #(#variant_defs),*
                    }

                    impl AstNode for #ast_name {
                        type Syntax = #lang_ident;
                        const KINDS: &[Self::Syntax] = &[ #(#kind_consts),* ];

                        fn cast(syntax: &SyntaxNode<Self::Syntax>) -> Option<Self> {
                            #(#cast_arms)*
                            None
                        }

                        fn syntax(&self) -> &SyntaxNode<Self::Syntax> {
                            match self {
                                #(#syntax_arms),*
                            }
                        }
                    }
                }
            }

            AstShape::Pratt { atom, prefix_ops, infix_ops } => {
                let prefix_enum = format_ident!("{}PrefixOp", name);
                let infix_enum = format_ident!("{}InfixOp", name);
                let prefix_expr = format_ident!("{}PrefixExpr", name);
                let infix_expr = format_ident!("{}InfixExpr", name);

                quote! {
                    #[derive(Clone)]
                    pub enum #ast_name {
                        #atom(#atom),
                        Prefix(#prefix_expr),
                        Infix(#infix_expr),
                    }

                    impl AstNode for #ast_name {
                        type Syntax = #lang_ident;
                        const KINDS: &[Self::Syntax] = &[#lang_ident::#name];

                        fn cast(syntax: &SyntaxNode<Self::Syntax>) -> Option<Self> {
                            Some(Self::#name(Self(syntax.clone())))
                        }

                        fn syntax(&self) -> &SyntaxNode<Self::Syntax> {
                            match self {
                                #ast_name::#atom(inner) => inner.syntax(),
                                #ast_name::Prefix(inner) => inner.rhs.syntax(),
                                #ast_name::Infix(inner) => inner.lhs.syntax(),
                            }
                        }
                    }

                    #[derive(Clone)]
                    pub struct #prefix_expr {
                        pub op: #prefix_enum,
                        pub rhs: Box<#ast_name>,
                    }

                    #[derive(Clone)]
                    pub struct #infix_expr {
                        pub lhs: Box<#ast_name>,
                        pub op: #infix_enum,
                        pub rhs: Box<#ast_name>,
                    }

                    #[derive(Clone, Copy)]
                    pub enum #prefix_enum {
                        #(#prefix_ops,)*
                    }

                    #[derive(Clone, Copy)]
                    pub enum #infix_enum {
                        #(#infix_ops,)*
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::derive::parser::DslExpr;
    use proc_macro2::Span;
    use std::collections::HashMap;

    fn ident(name: &str) -> Ident {
        Ident::new(name, Span::call_site())
    }

    fn just(name: &str) -> DslExpr {
        DslExpr::Just(ident(name))
    }

    #[test]
    fn just_expr_becomes_node() {
        let mut ctx = AstGenCtx::new();
        let mut ast_shapes = HashMap::new();
        let shape = just("A").ast_shape(&ident("Rule"), &mut ctx, &mut ast_shapes);

        match shape {
            AstShape::Node { fields } => {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].kind, ident("A"));
                assert!(ast_shapes.is_empty());
            }
            _ => panic!("expected Node"),
        }
    }

    #[test]
    fn seq_becomes_node_with_fields() {
        let mut ctx = AstGenCtx::new();
        let mut ast_shapes = HashMap::new();
        let expr = DslExpr::Seq(vec![just("A"), just("B"), just("C")]);
        let shape = expr.ast_shape(&ident("SeqRule"), &mut ctx, &mut ast_shapes);

        match shape {
            AstShape::Node { fields } => {
                assert_eq!(fields.len(), 3);
                assert_eq!(fields[0].kind, ident("A"));
                assert_eq!(fields[1].kind, ident("B"));
                assert_eq!(fields[2].kind, ident("C"));
                assert!(ast_shapes.is_empty());
            }
            _ => panic!("expected Node"),
        }
    }

    #[test]
    fn alt_of_simple_refs_becomes_enum() {
        let mut ctx = AstGenCtx::new();
        let mut ast_shapes = HashMap::new();
        let expr = DslExpr::Alt(vec![just("X"), just("Y")]);
        let shape = expr.ast_shape(&ident("Choice"), &mut ctx, &mut ast_shapes);

        match shape {
            AstShape::Enum { variants } => {
                assert_eq!(variants, vec![ident("X"), ident("Y")]);
                assert!(ast_shapes.is_empty());
            }
            _ => panic!("expected Enum"),
        }
    }

    #[test]
    fn alt_with_nested_seq_generates_helper() {
        let mut ctx = AstGenCtx::new();
        let mut ast_shapes = HashMap::new();
        let nested = DslExpr::Seq(vec![just("A"), just("B")]);
        let expr = DslExpr::Alt(vec![just("X"), nested]);
        let shape = expr.ast_shape(&ident("Choice"), &mut ctx, &mut ast_shapes);

        match shape {
            AstShape::Enum { variants } => {
                assert_eq!(variants.len(), 2);
                assert_eq!(variants[0], ident("X"));
                assert!(variants[1].to_string().starts_with("Choice_Alt_"));
            }
            _ => panic!("expected Enum"),
        }

        assert_eq!(ast_shapes.len(), 1);
        let (helper_name, helper_shape) = ast_shapes.iter().next().unwrap();
        assert!(helper_name.to_string().starts_with("Choice_Alt_"));
        match helper_shape {
            AstShape::Node { fields } => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].kind, ident("A"));
                assert_eq!(fields[1].kind, ident("B"));
            }
            _ => panic!("expected helper Node"),
        }
    }

    #[test]
    fn option_and_repeats_change_cardinality() {
        let mut ctx = AstGenCtx::new();
        let mut ast_shapes = HashMap::new();
        let opt = DslExpr::Opt(Box::new(just("X")));
        let star = DslExpr::Star(Box::new(just("Y")));
        let plus = DslExpr::Plus(Box::new(just("Z")));

        let opt_shape = opt.ast_shape(&ident("OptRule"), &mut ctx, &mut ast_shapes);
        let star_shape = star.ast_shape(&ident("StarRule"), &mut ctx, &mut ast_shapes);
        let plus_shape = plus.ast_shape(&ident("PlusRule"), &mut ctx, &mut ast_shapes);

        if let AstShape::Node { fields, .. } = opt_shape {
            assert_eq!(fields[0].cardinality, Cardinality::Optional);
        } else {
            panic!("expected Node");
        }

        if let AstShape::Node { fields, .. } = star_shape {
            assert_eq!(fields[0].cardinality, Cardinality::Repeated);
        } else {
            panic!("expected Node");
        }

        if let AstShape::Node { fields, .. } = plus_shape {
            assert_eq!(fields[0].cardinality, Cardinality::Repeated);
        } else {
            panic!("expected Node");
        }
    }
}
