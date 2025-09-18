use std::{
    collections::{HashMap, HashSet},
    mem,
};
use syn::Ident;

use crate::derive::attributes::Rule;

pub struct RuleGraph {
    pub adj: HashMap<Ident, HashSet<Ident>>, // dependency graph
}

pub struct RecursionInfo {
    pub components: Vec<HashSet<Ident>>,
    pub node_to_comp: HashMap<Ident, usize>,
    pub left_recursive: HashSet<Ident>,
    pub adj: HashMap<Ident, HashSet<Ident>>,
}

impl RecursionInfo {
    pub fn peers(&self, node: &Ident) -> impl Iterator<Item = &Ident> {
        self.node_to_comp
            .get(node)
            .into_iter()
            .flat_map(move |&comp| self.components[comp].iter().filter(move |n| *n != node))
    }

    pub fn is_left_recursive(&self, node: &Ident) -> bool {
        self.left_recursive.contains(node)
    }
}
impl RuleGraph {
    pub fn new() -> Self {
        Self {
            adj: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, name: Ident, deps: HashSet<Ident>) {
        self.adj.entry(name).or_default().extend(deps);
    }

    pub(crate) fn into_recursive_info(&mut self, rules: &HashMap<Ident, &Rule>) -> RecursionInfo {
        let (components, node_to_comp) = compute_sccs(&self.adj);
        let left_recursive = self.detect_left_recursion(rules);

        RecursionInfo {
            components,
            node_to_comp,
            left_recursive,
            adj: mem::take(&mut self.adj),
        }
    }

    fn compute_nullable(rules: &HashMap<Ident, &Rule>) -> HashMap<Ident, bool> {
        let mut nullable: HashMap<Ident, bool> =
            rules.keys().cloned().map(|k| (k, false)).collect();

        let mut changed = true;
        while changed {
            changed = false;
            for (name, rule) in rules {
                let new_val = rule.dsl.nullable_with(&nullable);
                if new_val && !*nullable.get(name).unwrap() {
                    nullable.insert(name.clone(), true);
                    changed = true;
                }
            }
        }
        nullable
    }

    fn build_left_corner_graph(
        rules: &HashMap<Ident, &Rule>,
        nullable: &HashMap<Ident, bool>,
    ) -> HashMap<Ident, HashSet<Ident>> {
        let mut lc: HashMap<Ident, HashSet<Ident>> =
            rules.keys().cloned().map(|k| (k, HashSet::new())).collect();

        for (a, rule) in rules {
            let mut out = HashSet::new();
            let mut visiting = HashSet::new();
            rule.dsl.first_nonterminals_with(
                a, // <── origin
                nullable,
                rules,
                &HashMap::new(),
                &mut out,
                &mut visiting,
            );
            lc.get_mut(a).unwrap().extend(out);
        }

        lc
    }

    fn detect_left_recursion(&self, rules: &HashMap<Ident, &Rule>) -> HashSet<Ident> {
        let nullable = Self::compute_nullable(rules);
        let lc_adj = Self::build_left_corner_graph(rules, &nullable);
        let (lc_components, _) = compute_sccs(&lc_adj);
        lc_components.into_iter().flatten().collect()
    }
}

fn compute_sccs(
    adj: &HashMap<Ident, HashSet<Ident>>,
) -> (Vec<HashSet<Ident>>, HashMap<Ident, usize>) {
    let mut components = Vec::new();
    let mut node_to_comp = HashMap::new();

    // 1. Assign each node an integer ID
    let nodes: Vec<Ident> = adj.keys().cloned().collect();
    let node_ids: HashMap<Ident, usize> = nodes
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, n)| (n, i))
        .collect();

    let n = nodes.len();
    let mut index = 0;
    let mut indices = vec![None; n];
    let mut lowlink = vec![0; n];
    let mut on_stack = vec![false; n];
    let mut stack = Vec::new();

    fn strongconnect(
        v: usize,
        adj: &HashMap<Ident, HashSet<Ident>>,
        nodes: &Vec<Ident>,
        node_ids: &HashMap<Ident, usize>,
        index: &mut usize,
        indices: &mut Vec<Option<usize>>,
        lowlink: &mut Vec<usize>,
        stack: &mut Vec<usize>,
        on_stack: &mut Vec<bool>,
        components: &mut Vec<HashSet<Ident>>,
        node_to_comp: &mut HashMap<Ident, usize>,
    ) {
        indices[v] = Some(*index);
        lowlink[v] = *index;
        *index += 1;
        stack.push(v);
        on_stack[v] = true;

        let v_name = &nodes[v];
        if let Some(neighbors) = adj.get(v_name) {
            for w_name in neighbors {
                if let Some(&w) = node_ids.get(w_name) {
                    if indices[w].is_none() {
                        strongconnect(
                            w,
                            adj,
                            nodes,
                            node_ids,
                            index,
                            indices,
                            lowlink,
                            stack,
                            on_stack,
                            components,
                            node_to_comp,
                        );
                        lowlink[v] = lowlink[v].min(lowlink[w]);
                    } else if on_stack[w] {
                        lowlink[v] = lowlink[v].min(indices[w].unwrap());
                    }
                }
            }
        }

        if lowlink[v] == indices[v].unwrap() {
            let mut component = HashSet::new();
            while let Some(w) = stack.pop() {
                on_stack[w] = false;
                component.insert(nodes[w].clone());
                if w == v {
                    break;
                }
            }
            let v_name = &nodes[v];
            let self_loop = adj.get(v_name).map_or(false, |deps| deps.contains(v_name));

            if component.len() > 1 || self_loop {
                let comp_idx = components.len();
                for node in &component {
                    node_to_comp.insert(node.clone(), comp_idx);
                }
                components.push(component);
            }
        }
    }

    for v in 0..n {
        if indices[v].is_none() {
            strongconnect(
                v,
                adj,
                &nodes,
                &node_ids,
                &mut index,
                &mut indices,
                &mut lowlink,
                &mut stack,
                &mut on_stack,
                &mut components,
                &mut node_to_comp,
            );
        }
    }

    (components, node_to_comp)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::derive::parser::*;
    use proc_macro2::{Ident, Span};

    fn ident(name: &str) -> Ident {
        Ident::new(name, Span::call_site())
    }

    fn just(name: &str) -> DslExpr {
        DslExpr::Just(ident(name))
    }

    fn rule(name: &str, expr: DslExpr) -> (Ident, Rule) {
        (
            ident(name),
            Rule {
                name: ident(name),
                parameters: HashSet::new(),
                dsl: expr,
            },
        )
    }

    fn make_rules(rules: Vec<(&str, DslExpr)>) -> HashMap<Ident, Rule> {
        rules.into_iter().map(|(n, e)| rule(n, e)).collect()
    }

    fn wrap<'a>(rules: &'a HashMap<Ident, Rule>) -> HashMap<Ident, &'a Rule> {
        rules.iter().map(|(k, v)| (k.clone(), v)).collect()
    }
    #[test]
    fn test_no_recursion() {
        let rules = make_rules(vec![
            ("A", just("B")),
            ("B", just("C")),
            ("C", just("t")), // terminal
        ]);
        let mut rg = RuleGraph::new();
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(info.components.is_empty());
        assert!(info.left_recursive.is_empty());
    }

    #[test]
    fn test_direct_left_recursion() {
        let rules = make_rules(vec![("A", DslExpr::Alt(vec![just("A"), just("t")]))]);
        let mut rg = RuleGraph::new();
        rg.add_rule(ident("A"), [ident("A")].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(info.is_left_recursive(&ident("A")));
    }

    #[test]
    fn test_guarded_recursion_not_left() {
        // A -> 't' A   (right recursion, safe)
        let rules = make_rules(vec![("A", DslExpr::Seq(vec![just("t"), just("A")]))]);
        let mut rg = RuleGraph::new();
        rg.add_rule(ident("A"), [ident("A")].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(!info.is_left_recursive(&ident("A")));
    }

    #[test]
    fn test_mutual_left_recursion() {
        // A -> B
        // B -> A
        let rules = make_rules(vec![("A", just("B")), ("B", just("A"))]);
        let mut rg = RuleGraph::new();
        rg.add_rule(ident("A"), [ident("B")].into_iter().collect());
        rg.add_rule(ident("B"), [ident("A")].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(info.is_left_recursive(&ident("A")));
        assert!(info.is_left_recursive(&ident("B")));
        assert_eq!(info.components.len(), 1); // one SCC
    }

    #[test]
    fn test_indirect_left_recursion() {
        // A -> B
        // B -> C
        // C -> A | t
        let rules = make_rules(vec![
            ("A", just("B")),
            ("B", just("C")),
            ("C", DslExpr::Alt(vec![just("A"), just("t")])),
        ]);
        let mut rg = RuleGraph::new();
        rg.add_rule(ident("A"), [ident("B")].into_iter().collect());
        rg.add_rule(ident("B"), [ident("C")].into_iter().collect());
        rg.add_rule(ident("C"), [ident("A")].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(info.is_left_recursive(&ident("A")));
        assert!(info.is_left_recursive(&ident("B")));
        assert!(info.is_left_recursive(&ident("C")));
    }

    #[test]
    fn test_nullable_path_allows_left_recursion() {
        // A -> ε | A
        let rules = make_rules(vec![(
            "A",
            DslExpr::Alt(vec![DslExpr::Opt(Box::new(just("eps"))), just("A")]),
        )]);
        let mut rg = RuleGraph::new();
        rg.add_rule(ident("A"), [ident("A")].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(info.is_left_recursive(&ident("A")));
    }

    #[test]
    fn test_right_recursion_through_other_rule() {
        // A -> t B
        // B -> B | t
        let rules = make_rules(vec![
            ("A", DslExpr::Seq(vec![just("t"), just("B")])),
            ("B", DslExpr::Alt(vec![just("B"), just("t")])),
        ]);
        let mut rg = RuleGraph::new();
        rg.add_rule(ident("A"), [ident("B")].into_iter().collect());
        rg.add_rule(ident("B"), [ident("B")].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));
        assert!(!info.is_left_recursive(&ident("A"))); // safe
        assert!(info.is_left_recursive(&ident("B"))); // left-recursive self
    }

    #[test]
    fn test_direct_param_recursion() {
        // A<T> = A<T> | T
        let a_ident = ident("A");
        let rules = make_rules(vec![(
            "A",
            DslExpr::Alt(vec![
                DslExpr::Call {
                    name: a_ident.clone(),
                    args: vec![just("T")],
                },
                just("T"),
            ]),
        )]);

        let mut rg = RuleGraph::new();
        rg.add_rule(a_ident.clone(), [a_ident.clone()].into_iter().collect());
        let info = rg.into_recursive_info(&wrap(&rules));

        assert!(info.is_left_recursive(&a_ident));
    }

    #[test]
    fn test_mutual_param_recursion() {
        // A<T> = B<T>
        // B<T> = A<T>
        let a_ident = ident("A");
        let b_ident = ident("B");

        let rules = make_rules(vec![
            (
                "A",
                DslExpr::Call {
                    name: b_ident.clone(),
                    args: vec![just("T")],
                },
            ),
            (
                "B",
                DslExpr::Call {
                    name: a_ident.clone(),
                    args: vec![just("T")],
                },
            ),
        ]);

        let mut rg = RuleGraph::new();
        rg.add_rule(a_ident.clone(), [b_ident.clone()].into_iter().collect());
        rg.add_rule(b_ident.clone(), [a_ident.clone()].into_iter().collect());

        let info = rg.into_recursive_info(&wrap(&rules));

        assert!(info.is_left_recursive(&a_ident));
        assert!(info.is_left_recursive(&b_ident));
    }
    #[test]
    fn test_indirect_param_recursion_three_step() {
        // A<T> = C<A<T>>
        // C<X> = X
        let a_ident = ident("A");
        let c_ident = ident("C");
        let t_ident = ident("T");
        let x_ident = ident("X");

        // A<T> body: C< A<T> >
        let a_body = DslExpr::Call {
            name: c_ident.clone(),
            args: vec![DslExpr::Call {
                name: a_ident.clone(),
                args: vec![DslExpr::Just(t_ident.clone())],
            }],
        };

        // C<X> body: X
        let c_body = DslExpr::Just(x_ident.clone());

        // Build rules with proper parameter sets
        let mut rules: HashMap<Ident, Rule> = HashMap::new();

        let mut a_params = HashSet::new();
        a_params.insert(t_ident.clone());
        rules.insert(
            a_ident.clone(),
            Rule {
                name: a_ident.clone(),
                parameters: a_params,
                dsl: a_body,
            },
        );

        let mut c_params = HashSet::new();
        c_params.insert(x_ident.clone());
        rules.insert(
            c_ident.clone(),
            Rule {
                name: c_ident.clone(),
                parameters: c_params,
                dsl: c_body,
            },
        );

        // Left-recursion detection only depends on the left-corner graph built from `rules`,
        // so we don't need to add edges to RuleGraph.adj for this assertion.
        let mut rg = RuleGraph::new();
        let info = rg.into_recursive_info(&wrap(&rules));

        assert!(info.is_left_recursive(&a_ident));
    }

    #[test]
    fn test_mutual_param_recursion_rule() {
        // A<T> = B<T>
        // B<U> = A<U>
        let a_ident = ident("A");
        let b_ident = ident("B");

        let rules = make_rules(vec![
            (
                "A",
                DslExpr::Call {
                    name: b_ident.clone(),
                    args: vec![just("T")],
                },
            ),
            (
                "B",
                DslExpr::Call {
                    name: a_ident.clone(),
                    args: vec![just("U")],
                },
            ),
        ]);

        let mut rg = RuleGraph::new();
        rg.add_rule(a_ident.clone(), [b_ident.clone()].into_iter().collect());
        rg.add_rule(b_ident.clone(), [a_ident.clone()].into_iter().collect());

        let info = rg.into_recursive_info(&wrap(&rules));

        assert!(info.is_left_recursive(&a_ident));
        assert!(info.is_left_recursive(&b_ident));
        assert_eq!(info.components.len(), 1); // both in same SCC
    }

    #[test]
    fn test_guarded_param_recursion_not_left() {
        // A<T> = t A<T>  (right recursion, safe)
        let a_ident = ident("A");

        let rules = make_rules(vec![(
            "A",
            DslExpr::Seq(vec![
                just("t"),
                DslExpr::Call {
                    name: a_ident.clone(),
                    args: vec![just("T")],
                },
            ]),
        )]);

        let mut rg = RuleGraph::new();
        rg.add_rule(a_ident.clone(), [a_ident.clone()].into_iter().collect());

        let info = rg.into_recursive_info(&wrap(&rules));

        assert!(!info.is_left_recursive(&a_ident));
    }
}
