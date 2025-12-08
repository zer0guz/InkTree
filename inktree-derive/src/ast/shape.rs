use crate::{
    language::{Language, LanguageElement},
    {
        attributes::{Pratt, SyntaxAttribute},
        parser::DslExpr,
    },
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::hash::{Hash, Hasher};
use std::{
    collections::{HashMap, HashSet},
    hash::DefaultHasher,
};
use syn::Ident;

// ===== IR =====

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
pub enum Cardinality {
    One,
    Optional,
    Repeated,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Item {
    Named(syn::Ident),
    Inline(Box<Shape>),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Member {
    pub label: syn::Ident,
    pub cardinality: Cardinality,
    pub item: Item,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Shape {
    Pratt {
        atom: Item,
        prefix_ops: Vec<syn::Ident>,
        infix_ops: Vec<syn::Ident>,
        postfix_ops: Vec<syn::Ident>,
    },
    Struct {
        members: Vec<Member>,
    },
    Enum {
        members: Vec<Member>,
    },
    Token(syn::Ident),
}

// ===== recursion guard key (no big clones) =====

#[derive(Clone, Eq, PartialEq, Hash)]
struct CallKey {
    callee: Ident,
    args_fp: u64, // fingerprint of args (structural hash)
}

fn fingerprint_args(args: &[DslExpr]) -> u64 {
    let mut h = DefaultHasher::new();
    for a in args {
        a.hash(&mut h);
    }
    h.finish()
}

// ===== lowering ctx =====

pub struct LowerCtx<'a> {
    lang: &'a Language,
    fresh: u32,
    visiting: HashSet<CallKey>, // guards param-recursive instantiations
    prune_ignored_tokens: bool, // ignore is only for tokens
}

impl<'a> LowerCtx<'a> {
    pub(crate) fn new(lang: &'a Language) -> Self {
        Self {
            lang,
            fresh: 0,
            visiting: HashSet::new(),
            prune_ignored_tokens: true,
        }
    }

    // ---------- labels ----------
    fn fresh_label(&mut self, owner: &Ident, kind: &str) -> Ident {
        let n = self.fresh;
        self.fresh += 1;
        Ident::new(&format!("{}_{}_{}", owner, kind, n), owner.span())
    }
    fn field_label_from_type(&self, ty: &Ident) -> Ident {
        to_snake_ident(ty)
    }
    fn variant_label_from_type(&self, ty: &Ident) -> Ident {
        ty.clone()
    }

    // ---------- pruning: tokens only ----------
    fn is_ignored_token(&self, id: &Ident) -> bool {
        match self.lang.idents.get(id) {
            Some(h) => {
                let el = &self.lang.element_pool[*h];
                el.rule().is_none() && !el.is_ast_relevant()
            }
            None => false,
        }
    }

    // ---------- public entry points ----------
    pub fn lower_rule_dsl(&mut self, owner: &Ident, dsl: &DslExpr) -> Option<Shape> {
        self.lower_shape(owner, dsl)
    }

    pub(crate) fn lower_pratt(&mut self, pratt: &Pratt) -> Option<Shape> {
        let owner = pratt.name();
        let atom = self.lower_item(owner, &pratt.node.0.dsl)?;
        let prefix_ops = self
            .lang
            .operators
            .iter()
            .filter(|o| o.is_prefix())
            .map(|o| o.ident.clone())
            .collect();
        let infix_ops = self
            .lang
            .operators
            .iter()
            .filter(|o| o.is_infix())
            .map(|o| o.ident.clone())
            .collect();
        let postfix_ops = self
            .lang
            .operators
            .iter()
            .filter(|o| o.is_postfix())
            .map(|o| o.ident.clone())
            .collect();
        Some(Shape::Pratt {
            atom,
            prefix_ops,
            infix_ops,
            postfix_ops,
        })
    }

    // ---------- core lowering ----------
    fn lower_shape(&mut self, owner: &Ident, e: &DslExpr) -> Option<Shape> {
        use DslExpr::*;
        match e {
            Alt(items) => {
                let mut members = Vec::with_capacity(items.len());
                for it in items {
                    if let Some(m) = self.lower_member_variant(owner, it, Cardinality::One) {
                        members.push(m);
                    }
                }
                if members.is_empty() {
                    return None;
                }
                self.dedupe_variant_labels(owner, &mut members);
                if members.len() == 1 {
                    self.collapse_single_variant(owner, members.pop().unwrap())
                } else {
                    Some(Shape::Enum { members })
                }
            }
            Seq(items) => {
                let mut members = Vec::with_capacity(items.len());
                for it in items {
                    if let Some(m) = self.lower_member_field(owner, it, Cardinality::One) {
                        members.push(m);
                    }
                }
                if members.is_empty() {
                    None
                } else {
                    self.dedupe_field_labels(owner, &mut members);
                    Some(Shape::Struct { members })
                }
            }
            Opt(inner) => self
                .lower_member_field(owner, inner, Cardinality::Optional)
                .map(|m| Shape::Struct { members: vec![m] }),
            Star(inner) | Plus(inner) => self
                .lower_member_field(owner, inner, Cardinality::Repeated)
                .map(|m| Shape::Struct { members: vec![m] }),
            Just(id) => {
                if self.prune_ignored_tokens && self.is_ignored_token(id) {
                    return None;
                }
                Some(Shape::Struct {
                    members: vec![Member {
                        label: self.field_label_from_type(id),
                        cardinality: Cardinality::One,
                        item: Item::Named(id.clone()),
                    }],
                })
            }
            Call { name, args } => {
                let item = self.lower_call_item(owner, name, args)?;
                Some(Shape::Struct {
                    members: vec![Member {
                        label: self.field_label_from_type(name),
                        cardinality: Cardinality::One,
                        item,
                    }],
                })
            }
        }
    }

    fn lower_item(&mut self, owner: &Ident, e: &DslExpr) -> Option<Item> {
        use DslExpr::*;
        match e {
            Just(id) => {
                if self.prune_ignored_tokens && self.is_ignored_token(id) {
                    None
                } else {
                    Some(Item::Named(id.clone()))
                }
            }
            Call { name, args } => self.lower_call_item(owner, name, args),
            _ => {
                let s = self.lower_shape(owner, e)?;
                if let Some(named) = s.peel_named() {
                    Some(Item::Named(named))
                } else {
                    Some(Item::Inline(Box::new(s)))
                }
            }
        }
    }

    fn lower_member_field(
        &mut self,
        owner: &Ident,
        e: &DslExpr,
        card: Cardinality,
    ) -> Option<Member> {
        self.lower_member(owner, e, card, /*variant=*/ false)
    }
    fn lower_member_variant(
        &mut self,
        owner: &Ident,
        e: &DslExpr,
        card: Cardinality,
    ) -> Option<Member> {
        self.lower_member(owner, e, card, /*variant=*/ true)
    }

    fn lower_member(
        &mut self,
        owner: &Ident,
        e: &DslExpr,
        card: Cardinality,
        variant: bool,
    ) -> Option<Member> {
        use DslExpr::*;
        match e {
            Opt(inner) => return self.lower_member(owner, inner, Cardinality::Optional, variant),
            Star(inner) => return self.lower_member(owner, inner, Cardinality::Repeated, variant),
            Plus(inner) => return self.lower_member(owner, inner, Cardinality::Repeated, variant),
            _ => {}
        }

        let label_for_named = if variant {
            |this: &LowerCtx, id: &Ident| this.variant_label_from_type(id)
        } else {
            |this: &LowerCtx, id: &Ident| this.field_label_from_type(id)
        };
        let label_fresh =
            |this: &mut LowerCtx, owner: &Ident, kind: &str| this.fresh_label(owner, kind);

        match e {
            Just(id) => {
                if self.prune_ignored_tokens && self.is_ignored_token(id) {
                    return None;
                }
                Some(Member {
                    label: label_for_named(self, id),
                    cardinality: card,
                    item: Item::Named(id.clone()),
                })
            }
            Call { name, args } => {
                let item = self.lower_call_item(owner, name, args)?;
                let label_id = if variant {
                    match &item {
                        Item::Named(inner) => inner,
                        _ => name,
                    }
                } else {
                    name
                };

                Some(Member {
                    label: label_for_named(self, label_id),
                    cardinality: card,
                    item,
                })
            }
            Alt(_) | Seq(_) => {
                let label = label_fresh(self, owner, if variant { "Var" } else { "field" });
                let sub = self.lower_shape(owner, e)?;
                Some(Member {
                    label,
                    cardinality: card,
                    item: Item::Inline(Box::new(sub)),
                })
            }
            Opt(_) | Star(_) | Plus(_) => unreachable!("normalized above"),
        }
    }

    // prevent enum variant label collisions
    fn dedupe_variant_labels(&mut self, owner: &Ident, members: &mut [Member]) {
        let mut used = std::collections::HashSet::<String>::new();
        for m in members.iter_mut() {
            let base = m.label.to_string();
            if used.insert(base.clone()) {
                continue;
            }
            let mut i = 2usize;
            loop {
                let cand = format!("{}{}", base, i);
                if used.insert(cand.clone()) {
                    m.label = Ident::new(&cand, owner.span());
                    break;
                }
                i += 1;
            }
        }
    }

    // ensure struct field labels are unique after pruning/normalization
    fn dedupe_field_labels(&mut self, owner: &Ident, members: &mut [Member]) {
        let mut used = std::collections::HashSet::<String>::new();
        for m in members.iter_mut() {
            let base = m.label.to_string();
            if used.insert(base.clone()) {
                continue;
            }
            let mut i = 2usize;
            loop {
                let cand = format!("{}{}", base, i);
                if used.insert(cand.clone()) {
                    m.label = Ident::new(&cand, owner.span());
                    break;
                }
                i += 1;
            }
        }
    }

    // collapse a 1-variant enum; preserve cardinality when not One
    fn collapse_single_variant(&mut self, owner: &Ident, v: Member) -> Option<Shape> {
        let shape = match v.item {
            Item::Named(id) => Shape::Struct {
                members: vec![Member {
                    label: self.field_label_from_type(&id),
                    cardinality: Cardinality::One,
                    item: Item::Named(id),
                }],
            },
            Item::Inline(s) => *s,
        };
        Some(match v.cardinality {
            Cardinality::One => shape,
            Cardinality::Optional | Cardinality::Repeated => Shape::Struct {
                members: vec![Member {
                    label: self.fresh_label(owner, "field"),
                    cardinality: v.cardinality,
                    item: Item::Inline(Box::new(shape)),
                }],
            },
        })
    }

    // ---------- call handling with positional substitution ----------
    fn lower_call_item(&mut self, owner: &Ident, name: &Ident, args: &[DslExpr]) -> Option<Item> {
        let Some(&h) = self.lang.idents.get(name) else {
            // Unknown callee: treat as leaf
            return Some(Item::Named(name.clone()));
        };
        let el = &self.lang.element_pool[h];

        if let Some(rule) = el.rule() {
            let key = CallKey {
                callee: name.clone(),
                args_fp: fingerprint_args(args),
            };
            if !self.visiting.insert(key.clone()) {
                // Param-recursive instantiation: bail out to avoid infinite inline
                return Some(Item::Named(name.clone()));
            }

            assert_eq!(
                rule.parameters.len(),
                args.len(),
                "arity mismatch for {}",
                name
            );
            let body = rule.dsl.subst_params(&rule.parameters, args);
            let node = self.lower_shape(owner, &body)?;

            self.visiting.remove(&key);

            // 👇 NEW bit: if the inlined shape is just a struct around a single Named,
            // treat the call as if it directly produced that Named.
            if let Some(named) = node.peel_named() {
                Some(Item::Named(named))
            } else {
                Some(Item::Inline(Box::new(node)))
            }
        } else {
            if self.prune_ignored_tokens && self.is_ignored_token(name) {
                None
            } else {
                Some(Item::Named(name.clone()))
            }
        }
    }
}

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

// ===== deps collection + root driver =====

impl Shape {
    pub fn collect_deps(&self) -> Vec<Ident> {
        let mut out = Vec::new();
        self.collect_deps_into(&mut out);
        out
    }
    pub fn collect_deps_into(&self, out: &mut Vec<Ident>) {
        match self {
            Shape::Token(_) => {}
            Shape::Pratt {
                atom,
                prefix_ops,
                infix_ops,
                postfix_ops,
            } => {
                atom.collect_deps_into(out);
                out.extend(prefix_ops.iter().cloned());
                out.extend(infix_ops.iter().cloned());
                out.extend(postfix_ops.iter().cloned());
            }
            Shape::Struct { members } | Shape::Enum { members } => {
                for m in members {
                    m.item.collect_deps_into(out);
                }
            }
        }
    }
    pub fn peel_named(&self) -> Option<Ident> {
        match self {
            Shape::Struct { members } if members.len() == 1 => {
                let m = &members[0];
                if m.cardinality == Cardinality::One {
                    if let Item::Named(id) = &m.item {
                        return Some(id.clone());
                    }
                }
                None
            }
            _ => None,
        }
    }
}
impl Item {
    pub fn collect_deps_into(&self, out: &mut Vec<Ident>) {
        match self {
            Item::Named(id) => out.push(id.clone()),
            Item::Inline(shape) => shape.collect_deps_into(out),
        }
    }
}

// ===== reachable-set materialization from root =====

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: HashMap<Ident, Shape>,
    pub tokens: HashSet<Ident>,
    pub rules: HashMap<Ident, Shape>,
    pub anon_shapes: HashMap<Ident, Shape>,
}
impl Ast {
    pub(crate) fn build_from_root(lang: &mut Language, root: &Ident) -> Ast {
        fn fmt_ident_list(xs: &[Ident]) -> String {
            if xs.is_empty() {
                return "[]".to_string();
            }
            let mut s = String::from("[");
            for (i, id) in xs.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&id.to_string());
            }
            s.push(']');
            s
        }

        eprintln!("┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        eprintln!("┃ AST build start: root = {}", root);
        eprintln!("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        let mut out = Ast::default();
        let mut work = vec![root.clone()];
        let mut seen = std::collections::HashSet::<Ident>::new();
        let mut step = 0;

        while let Some(name) = work.pop() {
            step += 1;
            eprintln!("step #{:04} ─ pop {}", step, name);

            if !seen.insert(name.clone()) {
                eprintln!("  └─ already seen, skip");
                continue;
            }

            let handle = *lang.idents.get(&name).expect("language verified");
            let el = &lang.element_pool[handle];

            eprintln!("  ├─ lower '{}'", name);
            if let Some(shape) = el.attribute.ast_shape(lang) {
                match &shape {
                    Shape::Token(tok) => {
                        eprintln!("  ├─ kind = Token({}) -> add to tokens", tok);
                        out.tokens.insert(tok.clone());
                        eprintln!(
                            "  └─ counters: seen={}, tokens={}, nodes={}",
                            seen.len(),
                            out.tokens.len(),
                            out.nodes.len()
                        );
                    }
                    _ => {
                        let mut deps = shape.collect_deps();
                        eprintln!("  ├─ deps (raw)   = {}", fmt_ident_list(&deps));
                        deps.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                        deps.dedup();
                        eprintln!("  ├─ deps (dedup) = {}", fmt_ident_list(&deps));

                        let n_push = deps.len();
                        work.extend(deps);
                        out.nodes.insert(name.clone(), shape);

                        eprintln!("  ├─ pushed {} deps, work size now {}", n_push, work.len());
                        eprintln!(
                            "  └─ counters: seen={}, tokens={}, nodes={}",
                            seen.len(),
                            out.tokens.len(),
                            out.nodes.len()
                        );
                    }
                }
            } else {
                eprintln!("  └─ fully pruned (None) – nothing inserted");
                eprintln!(
                    "     counters: seen={}, tokens={}, nodes={}",
                    seen.len(),
                    out.tokens.len(),
                    out.nodes.len()
                );
            }
        }

        eprintln!("┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        eprintln!(
            "┃ AST build done: nodes={}, tokens={}",
            out.nodes.len(),
            out.tokens.len()
        );
        eprintln!("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        lang.ast_relevant = seen;
        out
    }

    pub(crate) fn codegen(&self, language: &crate::language::Language) -> TokenStream {
        let mut code = self.token_codegen(language);

        code.extend(self.node_codegen(&language));

        code
    }

    pub(crate) fn token_codegen(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;

        self.tokens
            .iter()
            .map(|name| {
                let ast_name = format_ident!("{}Ast", name);
                quote! {
                    ::inktree::ast_token_kind!(#lang_ident::#name => #ast_name);
                }
            })
            .collect()
    }

    pub(crate) fn node_codegen(&self, language: &Language) -> TokenStream {
        let lang_ident = &language.ident;

        self.nodes
                .iter()
                .map(|(name, shape)| {
                    let ast_name = format_ident!("{}Ast", name);
                    let ast_enum = format_ident!("{}AstEnum", name);
                    match shape {
                        Shape::Pratt {
                            atom,
                            prefix_ops,
                            infix_ops,
                            postfix_ops,
                        } => {
                            let atom_name = match atom {
                                Item::Named(ident) => {
                                    if let SyntaxAttribute::Rule(_rule) = &language.element_by_name(ident).attribute {
                                        unreachable!("aaah rule as pratt atom")
                                    };
                                    ident
                                },
                                // Item::Inline(inner_shape) => match inner_shape.as_ref() {
                                //     Shape::Struct { members: _ } => todo!("struct"),
                                //     Shape::Enum { members } => {
                                //         atom_name = format_ident!("{}AtomMarker", name);

                                //         let enum_variants = members.iter().map(|m| {
                                //             let name = &m.label;
                                //             use SyntaxAttribute::*;
                                //             let ty = match language.element_by_name(name).attribute {
                                //                 StaticToken(_) | Token(_) => quote! {Token},
                                //                 _ => quote! {Node},
                                //             };

                                //             quote! {#name:#ty}
                                //         });

                                //         quote! {
                                //             ::inktree::ast_node_anon_enum!(#lang_ident::#atom_name(#( #enum_variants| )*) => #atom_ast_name,#atom_ast_enum_name);

                                //         }
                                //     }
                                //     _ => unreachable!("inline shape cant be token/pratt"),
                                // },
                                _=> unreachable!("pratt has to be a single namend language element")
                            };
                            let prefix_op_ty = format_ident!("{}PrefixOp", name);
                            let infix_op_ty = format_ident!("{}InfixOp", name);
                            let postfix_op_ty = format_ident!("{}PostfixOp", name);
                            let prefix_expr = format_ident!("{}Prefix", name);
                            let infix_expr = format_ident!("{}Infix", name);
                            let postfix_expr  = format_ident!("{}Postfix", name);

                            let has_prefix = !prefix_ops.is_empty();
                            let has_infix  = !infix_ops.is_empty();
                            let has_postfix = !postfix_ops.is_empty();


                            // Variants for PrefixOp enum
                            let prefix_variants = prefix_ops.iter().map(|op| {
                                let variant = format_ident!("{}Ast", op); // e.g. PlusAst
                                let ty      = format_ident!("{}Ast", op); // payload type: PlusAst<S>
                                quote! { #variant(#ty<S>) }
                            });

                            // Variants for InfixOp enum
                            let infix_variants = infix_ops.iter().map(|op| {
                                let variant = format_ident!("{}Ast", op);
                                let ty      = format_ident!("{}Ast", op);
                                quote! { #variant(#ty<S>) }
                            });

                            // Variants for PostfixOp enum
                            let postfix_variants = postfix_ops.iter().map(|op| {
                                let variant = format_ident!("{}Ast", op); // e.g. PlusAst
                                let ty      = format_ident!("{}Ast", op); // payload type: PlusAst<S>
                                quote! { #variant(#ty<S>) }
                            });

                            // match arms for prefix operators
                            let prefix_op_match_arms = prefix_ops.iter().map(|op| {
                                let lang_kind   = op;                         // TestLang::Plus
                                let variant     = format_ident!("{}Ast", op); // ExprPrefixOp::PlusAst
                                quote! {
                                    #lang_ident::#lang_kind => #prefix_op_ty::#variant(first.clone().into())
                                }
                            });

                            // match arms for infix operators
                            let infix_op_match_arms = infix_ops.iter().map(|op| {
                                let lang_kind   = op;
                                let variant     = format_ident!("{}Ast", op);
                                quote! {
                                    #lang_ident::#lang_kind => #infix_op_ty::#variant(second.clone().into())
                                }
                            });

                            let postfix_op_match_arms = postfix_ops.iter().map(|op| {
                                let lang_kind = op;                         // e.g. Plus
                                let variant   = format_ident!("{}Ast", op); // PostfixOp::PlusAst
                                quote! {
                                    #lang_ident::#lang_kind => #postfix_op_ty::#variant(second.clone().into())
                                }
                            });

                            let prefix_defs = if has_prefix {
                                quote! {
                                    /// Operator enum for prefix expressions inside this Pratt node.
                                    #[derive(Debug)]
                                    pub enum #prefix_op_ty<S> {
                                        #( #prefix_variants, )*
                                    }

                                    /// Prefix expression payload for this Pratt node.
                                    #[derive(Debug)]
                                    pub struct #prefix_expr<S: ::inktree::State> {
                                        pub op: #prefix_op_ty<S>,
                                        pub rhs: #ast_name<S>,
                                    }
                                }
                            } else {
                                quote! {}
                            };

                            let infix_defs = if has_infix {
                                quote! {
                                    /// Operator enum for infix expressions inside this Pratt node.
                                    #[derive(Debug)]
                                    pub enum #infix_op_ty<S> {
                                        #( #infix_variants, )*
                                    }

                                    /// Infix expression payload for this Pratt node.
                                    #[derive(Debug)]
                                    pub struct #infix_expr<S: ::inktree::State> {
                                        pub lhs: #ast_name<S>,
                                        pub op:  #infix_op_ty<S>,
                                        pub rhs: #ast_name<S>,
                                    }
                                }
                            } else {
                                quote! {}
                            };

                            let postfix_defs = if has_postfix {
                                quote! {
                                    /// Operator enum for postfix expressions inside this Pratt node.
                                    #[derive(Debug)]
                                    pub enum #postfix_op_ty<S> {
                                        #( #postfix_variants, )*
                                    }

                                    /// Postfix expression payload for this Pratt node.
                                    #[derive(Debug)]
                                    pub struct #postfix_expr<S: ::inktree::State> {
                                        pub lhs: #ast_name<S>,
                                        pub op:  #postfix_op_ty<S>,
                                    }
                                }
                            } else {
                                quote! {}
                            };

                            let ast_enum_prefix_variant = if has_prefix {
                                quote! { Prefix(#prefix_expr<S>), }
                            } else {
                                quote! {}
                            };

                            let ast_enum_infix_variant = if has_infix {
                                quote! { Infix(#infix_expr<S>), }
                            } else {
                                quote! {}
                            };

                            let ast_enum_postfix_variant = if has_postfix {
                                quote! { Postfix(#postfix_expr<S>), }
                            } else {
                                quote! {}
                            };

                            let prefix_expr_body = if has_prefix {
                                quote! {
                                    Self::Prefix(#prefix_expr {
                                        op: match first.kind() {
                                            #( #prefix_op_match_arms, )*
                                            _ => unreachable!("unknown prefix operator kind for this Pratt node"),
                                        },
                                        rhs: parts
                                            .next()
                                            .expect("invalid pratt node: found a single operator without atom")
                                            .into_node()
                                            .expect("invalid pratt node: found operator and second child wasn't a node")
                                            .clone()
                                            .into(),
                                    })
                                }
                            } else {
                                quote! {
                                    unreachable!("prefix operators are not configured for this Pratt node")
                                }
                            };

                            let infix_expr_body = if has_infix {
                                quote! {
                                    Self::Infix(#infix_expr {
                                        lhs: first.clone().into(),
                                        op: match second.kind() {
                                            #( #infix_op_match_arms, )*
                                            _ => unreachable!("unknown infix operator kind for this Pratt node"),
                                        },
                                        rhs: third.clone().into(),
                                    })
                                }
                            } else {
                                quote! {
                                    unreachable!("infix operators are not configured for this Pratt node")
                                }
                            };

                            let postfix_expr_body = if has_postfix {
                                quote! {
                                    Self::Postfix(#postfix_expr {
                                        lhs: first.clone().into(),
                                        op: match second.kind() {
                                            #( #postfix_op_match_arms, )*
                                            _ => unreachable!("unknown postfix operator kind for this Pratt node"),
                                        },
                                    })
                                }
                            } else {
                                quote! {
                                    unreachable!("postfix operators are not configured for this Pratt node")
                                }
                            };

                            quote! {
                                //#atom_ast

                                ::inktree::ast_node_kind!(#lang_ident::#name => #ast_name,#ast_enum);
                                #[derive(Debug)]
                                pub enum #ast_enum<S: inktree::State> {
                                    Atom(::inktree::AstNodeWrapper<#atom_name, S, #lang_ident>),
                                    #ast_enum_prefix_variant
                                    #ast_enum_infix_variant
                                    #ast_enum_postfix_variant
                                }

                                #prefix_defs
                                #infix_defs
                                #postfix_defs


                                impl<S: ::inktree::State> ::inktree::View for #ast_enum<S> {
                                    type Kind = #name;
                                    type State = S;

                                    fn from_raw_token(_raw: ::inktree::SyntaxToken<<Self::Kind as ::inktree::Kind>::Syntax>) -> Self {
                                        unreachable!()
                                    }
                                    fn from_raw_node(
                                        raw: ::inktree::SyntaxNode<<Self::Kind as ::inktree::Kind>::Syntax>,
                                    ) -> Self {
                                        use ::inktree::cstree::util::NodeOrToken::*;
                                        let mut parts = ::inktree::significant_children_with_token(&raw);
                                        let first = parts
                                            .next()
                                            .expect("pratt node has to have atleast one child");

                                        match first {
                                            Node(first) => {
                                                match parts.next() {
                                                    Some(Token(second)) => match parts.next() {
                                                        Some(Node(third)) => {
                                                            // lhs op rhs  => infix
                                                            #infix_expr_body
                                                        }
                                                        None => {
                                                            // lhs op  => postfix
                                                            #postfix_expr_body
                                                        }
                                                        _ => unreachable!(),
                                                    },
                                                    None => {
                                                        // just nested Atom/Expr node
                                                        Self::Atom(first.clone().into())
                                                    }
                                                    _ => unreachable!(),
                                                }
                                            }
                                            Token(first) => {
                                                // op rhs => prefix
                                                #prefix_expr_body
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Shape::Struct { members } => {
                            let ext_trait = format_ident!("{}AstExt", name);

                            let mut sigs = Vec::new();
                            let mut impls = Vec::new();
                            let mut required_childs = TokenStream::new();

                            for m in members {
                                if let Some((sig, imp)) = gen_struct_member_accessor(&self.tokens, m) {
                                    sigs.push(sig);
                                    impls.push(imp);
                                }
                                match &m.item {
                                    Item::Named(ident) => match m.cardinality {
                                        Cardinality::One => {
                                            required_childs.extend(quote! {
                                                unsafe impl ::inktree::RequiredChild<#ident> for #name{}
                                            });
                                        }
                                        Cardinality::Optional => (),
                                        Cardinality::Repeated => todo!(),
                                    },
                                    Item::Inline(_shape) => todo!("fix me pls here"),
                                }
                            }

                            quote! {
                                ::inktree::ast_node_kind!(#lang_ident::#name => #ast_name);

                                #required_childs

                                pub trait #ext_trait<S: ::inktree::State> {
                                    #( #sigs )*
                                }

                                impl<S: ::inktree::State> #ext_trait<S> for #ast_name<S> {
                                    #( #impls )*
                                }
                            }
                        }
                        Shape::Token(_) => unreachable!("tokens are stored seperatly"),
                        Shape::Enum { members } => {
                            let enum_name = format_ident!("{}AstEnum", name);

                            // Enum payload types: Variant(FooAst<S>)
                            let enum_variants = members.iter().map(|m| {
                                let variant_label = &m.label; // deduped label (A, A2, KwPub, ...)
                                match &m.item {
                                    Item::Named(underlying) => {
                                        let payload_ty = format_ident!("{}Ast", underlying);
                                        quote! {
                                            #variant_label(#payload_ty<S>)
                                        }
                                    }
                                    Item::Inline(inline) => {
                                        eprintln!("Shape: {:#?}", shape);
                                        eprintln!("Inline: {:#?}", inline);
                                        panic!("inline enum variants are not yet supported in inktree AST codegen!!!");
                                    }
                                }
                            });

                            // Generate one `if let` arm per member, using `token::<...>()` or `child::<...>()`
                            let mut match_arms = Vec::new();
                            for (i, m) in members.iter().enumerate() {
                                let variant_label = &m.label;

                                let underlying = match &m.item {
                                    Item::Named(u) => u,
                                    Item::Inline(_) => unreachable!(),
                                };

                                let payload_ty = format_ident!("{}Ast", underlying);

                                // IMPORTANT: use the *underlying* element name to look up the attribute
                                let elem = language.element_by_name(underlying);
                                use SyntaxAttribute::*;

                                let arm = match elem.attribute {
                                    StaticToken(_) | Token(_) => {
                                        // Variant wraps a token → use `token::<PayloadAst<S>>`
                                        if i == 0 {
                                            quote! {
                                                if let Some(v) = ::inktree::token::<#payload_ty<S>>(&raw) {
                                                    Self::#variant_label(v)
                                                }
                                            }
                                        } else {
                                            quote! {
                                                else if let Some(v) = ::inktree::token::<#payload_ty<S>>(&raw) {
                                                    Self::#variant_label(v)
                                                }
                                            }
                                        }
                                    }
                                    _ => {
                                        // Variant wraps a node → use `child::<PayloadAst<S>>`
                                        if i == 0 {
                                            quote! {
                                                if let Some(v) = ::inktree::child::<#payload_ty<S>>(&raw) {
                                                    Self::#variant_label(v)
                                                }
                                            }
                                        } else {
                                            quote! {
                                                else if let Some(v) = ::inktree::child::<#payload_ty<S>>(&raw) {
                                                    Self::#variant_label(v)
                                                }
                                            }
                                        }
                                    }
                                };

                                match_arms.push(arm);
                            }

                            quote! {
                                ::inktree::ast_node_kind!(#lang_ident::#name => #ast_name, #enum_name);

                                /// View enum for the `#name` AST node.
                                #[derive(Debug)]
                                pub enum #enum_name<S: inktree::State> {
                                    #( #enum_variants, )*
                                }

                                impl<S: inktree::State> inktree::View for #enum_name<S> {
                                    type Kind = #name;
                                    type State = S;

                                    fn from_raw_token(
                                        _raw: inktree::SyntaxToken<<Self::Kind as inktree::Kind>::Syntax>,
                                    ) -> Self {
                                        unreachable!(concat!(stringify!(#name), " is always a node"));
                                    }

                                    fn from_raw_node(
                                        raw: inktree::SyntaxNode<<Self::Kind as inktree::Kind>::Syntax>,
                                    ) -> Self {
                                        #( #match_arms )*
                                        else {
                                            unreachable!("enum node only constructed if structurally valid");
                                        }
                                    }
                                }
                            }
                        }

                    }
                })
                .collect()
    }
}

// ===== helpers =====

fn to_snake_ident(src: &Ident) -> Ident {
    let s = src.to_string();
    let mut out = String::with_capacity(s.len() + 2);
    for (i, ch) in s.chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if i != 0 {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push(ch);
        }
    }
    match out.as_str() {
        "type" | "match" | "mod" | "ref" | "self" | "enum" | "struct" | "fn" => {
            Ident::new(&format!("r#{}", out), src.span())
        }
        _ => Ident::new(&out, src.span()),
    }
}

fn gen_struct_member_accessor(
    tokens: &std::collections::HashSet<Ident>,
    m: &Member,
) -> Option<(TokenStream, TokenStream)> {
    let field_name = &m.label;

    // Map Item::Named(T) -> TAst
    let (item_ty_ident, is_token) = match &m.item {
        Item::Named(id) => {
            let alias = format_ident!("{}Ast", id);
            let is_token = tokens.contains(id);
            (alias, is_token)
        }
        Item::Inline(_) => {
            // Skip anonymous inline shapes for now
            return None;
        }
    };

    let (sig, impl_) = match (m.cardinality, is_token) {
        (Cardinality::One, true) => struct_accessor_sig_impl!(req_token, field_name, item_ty_ident),
        (Cardinality::One, false) => struct_accessor_sig_impl!(req_node, field_name, item_ty_ident),
        (Cardinality::Optional, true) => {
            struct_accessor_sig_impl!(opt_token, field_name, item_ty_ident)
        }
        (Cardinality::Optional, false) => {
            struct_accessor_sig_impl!(opt_node, field_name, item_ty_ident)
        }
        (Cardinality::Repeated, true) => {
            struct_accessor_sig_impl!(rep_token, field_name, item_ty_ident)
        }
        (Cardinality::Repeated, false) => {
            struct_accessor_sig_impl!(rep_node, field_name, item_ty_ident)
        }
    };

    Some((sig, impl_))
}

#[cfg(test)]
mod tests {
    use crate::util::Pool;

    use super::*;
    use proc_macro2::Span;
    use std::collections::HashMap;

    // --- tiny DSL helpers ---
    fn ident(s: &str) -> Ident {
        Ident::new(s, Span::call_site())
    }
    fn just(s: &str) -> DslExpr {
        DslExpr::Just(ident(s))
    }
    fn seq(xs: Vec<DslExpr>) -> DslExpr {
        DslExpr::Seq(xs)
    }
    fn alt(xs: Vec<DslExpr>) -> DslExpr {
        DslExpr::Alt(xs)
    }
    fn opt(x: DslExpr) -> DslExpr {
        DslExpr::Opt(Box::new(x))
    }
    fn star(x: DslExpr) -> DslExpr {
        DslExpr::Star(Box::new(x))
    }
    fn plus(x: DslExpr) -> DslExpr {
        DslExpr::Plus(Box::new(x))
    }
    fn call(name: &str, args: Vec<DslExpr>) -> DslExpr {
        DslExpr::Call {
            name: ident(name),
            args,
        }
    }

    // --- minimal Language builder ---
    // Assumes your Pool<Element> and RuleGraph implement Default; if not, replace with your ctors.
    fn mk_lang_empty() -> Language {
        Language {
            element_pool: Pool::with_capacity(1),
            ident: ident("TestLang"),
            operators: vec![],
            root: None,
            cycle_graph: Default::default(),
            recursion_info: None,
            extras: vec![],
            idents: HashMap::new(),
            ast_relevant: HashSet::new(),
            error_token: None,
        }
    }

    // --- shape extractors ---
    fn expect_struct(shape: Option<Shape>) -> Vec<Member> {
        match shape {
            Some(Shape::Struct { members }) => members,
            other => panic!("expected Struct, got {:?}", other),
        }
    }
    fn expect_enum(shape: Option<Shape>) -> Vec<Member> {
        match shape {
            Some(Shape::Enum { members }) => members,
            other => panic!("expected Enum, got {:?}", other),
        }
    }

    // ================== TESTS ==================

    #[test]
    fn seq_basic_two_fields() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Owner");

        let members = expect_struct(lc.lower_rule_dsl(&owner, &seq(vec![just("A"), just("B")])));

        assert_eq!(members.len(), 2);
        assert_eq!(members[0].label.to_string(), "a");
        assert_eq!(members[1].label.to_string(), "b");
        assert_eq!(members[0].cardinality, Cardinality::One);
        assert_eq!(members[1].cardinality, Cardinality::One);
        matches!(&members[0].item, Item::Named(id) if *id == ident("A"));
        matches!(&members[1].item, Item::Named(id) if *id == ident("B"));
    }

    #[test]
    fn alt_single_collapses_to_struct() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Root");

        let members = expect_struct(lc.lower_rule_dsl(&owner, &alt(vec![just("A")])));

        assert_eq!(members.len(), 1);
        assert_eq!(members[0].label.to_string(), "a");
        matches!(&members[0].item, Item::Named(id) if *id == ident("A"));
    }

    #[test]
    fn alt_single_optional_preserves_cardinality_on_wrap() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Root");

        let members = expect_struct(lc.lower_rule_dsl(&owner, &alt(vec![opt(just("A"))])));

        assert_eq!(members.len(), 1);
        assert_eq!(members[0].cardinality, Cardinality::Optional);
        match &members[0].item {
            Item::Inline(inner) => match inner.as_ref() {
                Shape::Struct {
                    members: inner_members,
                } => {
                    assert_eq!(inner_members.len(), 1);
                    assert_eq!(inner_members[0].label.to_string(), "a");
                    assert_eq!(inner_members[0].cardinality, Cardinality::One);
                }
                other => panic!("expected inline collapsed struct, got {:?}", other),
            },
            other => panic!("expected inline, got {:?}", other),
        }
    }

    #[test]
    fn alt_single_repeated_preserves_cardinality_on_wrap() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Root");

        let members = expect_struct(lc.lower_rule_dsl(&owner, &alt(vec![plus(just("A"))])));

        assert_eq!(members.len(), 1);
        assert_eq!(members[0].cardinality, Cardinality::Repeated);
        match &members[0].item {
            Item::Inline(inner) => match inner.as_ref() {
                Shape::Struct {
                    members: inner_members,
                } => {
                    assert_eq!(inner_members.len(), 1);
                    assert_eq!(inner_members[0].label.to_string(), "a");
                    assert_eq!(inner_members[0].cardinality, Cardinality::One);
                }
                other => panic!("expected inline collapsed struct, got {:?}", other),
            },
            other => panic!("expected inline, got {:?}", other),
        }
    }

    #[test]
    fn field_labels_dedup_in_seq() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Root");

        let members = expect_struct(lc.lower_rule_dsl(&owner, &seq(vec![just("A"), just("A")])));

        assert_eq!(members.len(), 2);
        assert_eq!(members[0].label.to_string(), "a");
        assert_eq!(members[1].label.to_string(), "a2"); // dedup suffix
    }

    #[test]
    fn variant_labels_dedup_in_alt() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Root");

        let variants =
            expect_enum(lc.lower_rule_dsl(&owner, &alt(vec![just("A"), just("A"), just("A")])));

        assert_eq!(variants.len(), 3);
        assert_eq!(variants[0].label, ident("A"));
        assert_eq!(variants[1].label, ident("A2"));
        assert_eq!(variants[2].label, ident("A3"));
    }

    #[test]
    fn inline_anonymous_alt_inside_seq() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("Node");

        let members =
            expect_struct(lc.lower_rule_dsl(&owner, &seq(vec![alt(vec![just("A"), just("B")])])));

        assert_eq!(members.len(), 1);
        match &members[0].item {
            Item::Inline(inner) => match inner.as_ref() {
                Shape::Enum { members: variants } => {
                    assert_eq!(variants.len(), 2);
                    assert_eq!(variants[0].label, ident("A"));
                    assert_eq!(variants[1].label, ident("B"));
                }
                other => panic!("expected inline enum, got {:?}", other),
            },
            other => panic!("expected inline, got {:?}", other),
        }
    }

    #[test]
    fn cardinality_wrappers_normalize() {
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("X");

        let members = expect_struct(lc.lower_rule_dsl(
            &owner,
            &seq(vec![opt(just("A")), star(just("B")), plus(just("C"))]),
        ));

        assert_eq!(members.len(), 3);
        assert_eq!(members[0].label.to_string(), "a");
        assert_eq!(members[0].cardinality, Cardinality::Optional);
        assert_eq!(members[1].label.to_string(), "b");
        assert_eq!(members[1].cardinality, Cardinality::Repeated);
        assert_eq!(members[2].label.to_string(), "c");
        assert_eq!(members[2].cardinality, Cardinality::Repeated);
    }

    #[test]
    fn call_unknown_kept_as_named_leaf() {
        // No idents in Language => call("Foo", ...) is unknown => Named("Foo")
        let lang = mk_lang_empty();
        let mut lc = LowerCtx::new(&lang);
        let owner = ident("O");

        let members = expect_struct(lc.lower_rule_dsl(&owner, &call("Foo", vec![just("X")])));

        assert_eq!(members.len(), 1);
        assert_eq!(members[0].label.to_string(), "foo"); // snake_case label from type
        matches!(&members[0].item, Item::Named(id) if *id == ident("Foo"));
    }

    #[test]
    fn deps_collects_from_inline_and_named_and_pratt_ops() {
        // Build a composite shape manually to test collect_deps (no Language needed).
        let shape = Shape::Struct {
            members: vec![
                Member {
                    label: ident("f1"),
                    cardinality: Cardinality::One,
                    item: Item::Named(ident("T1")),
                },
                Member {
                    label: ident("f2"),
                    cardinality: Cardinality::One,
                    item: Item::Inline(Box::new(Shape::Enum {
                        members: vec![
                            Member {
                                label: ident("V1"),
                                cardinality: Cardinality::One,
                                item: Item::Named(ident("T2")),
                            },
                            Member {
                                label: ident("V2"),
                                cardinality: Cardinality::One,
                                item: Item::Inline(Box::new(Shape::Pratt {
                                    atom: Item::Named(ident("Atomish")),
                                    prefix_ops: vec![ident("Plus")],
                                    infix_ops: vec![ident("Minus")],
                                    postfix_ops: vec![ident("Bang")],
                                })),
                            },
                        ],
                    })),
                },
            ],
        };

        let mut deps = shape.collect_deps();
        deps.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
        assert_eq!(
            deps,
            vec![
                ident("Atomish"),
                ident("Bang"),
                ident("Minus"),
                ident("Plus"),
                ident("T1"),
                ident("T2"),
            ]
        );
    }
}
