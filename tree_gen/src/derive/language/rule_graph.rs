use std::collections::{HashMap, HashSet};

pub struct RuleGraph {
    adj: HashMap<String, HashSet<String>>,
}

impl RuleGraph {
    pub fn new() -> Self {
        Self {
            adj: HashMap::new(),
        }
    }

    pub fn add_edge(&mut self, from: String, to: String) {
        self.adj.entry(from.clone()).or_default().insert(to);
    }

    pub fn add_deps(&mut self, from: String, deps: impl IntoIterator<Item = String>) {
        self.adj.entry(from).or_default().extend(deps);
    }

    fn dfs(&self, node: &str, visited: &mut HashSet<String>, stack: &mut HashSet<String>) -> bool {
        if !visited.contains(node) {
            visited.insert(node.to_string());
            stack.insert(node.to_string());

            if let Some(neighbors) = self.adj.get(node) {
                for neighbor in neighbors {
                    if !visited.contains(neighbor) && self.dfs(neighbor, visited, stack) {
                        return true;
                    } else if stack.contains(neighbor) {
                        return true; // back-edge => cycle
                    }
                }
            }
        }
        stack.remove(node);
        false
    }

    pub fn recursive_components(&self) -> Vec<Vec<String>> {
        let mut index = 0usize;
        let mut indices = HashMap::new();
        let mut lowlink = HashMap::new();
        let mut stack = Vec::new();
        let mut on_stack = HashSet::new();
        let mut result = Vec::new();

        fn strongconnect<'a>(
            v: &'a str,
            index: &mut usize,
            indices: &mut HashMap<&'a str, usize>,
            lowlink: &mut HashMap<&'a str, usize>,
            stack: &mut Vec<&'a str>,
            on_stack: &mut HashSet<&'a str>,
            result: &mut Vec<Vec<String>>,
            graph: &'a RuleGraph,
        ) {
            indices.insert(v, *index);
            lowlink.insert(v, *index);
            *index += 1;
            stack.push(v);
            on_stack.insert(v);

            if let Some(neighbors) = graph.adj.get(v) {
                for w in neighbors {
                    let w = w.as_str();
                    if !indices.contains_key(w) {
                        strongconnect(w, index, indices, lowlink, stack, on_stack, result, graph);
                        let low_v = *lowlink.get(v).unwrap();
                        let low_w = *lowlink.get(w).unwrap();
                        lowlink.insert(v, low_v.min(low_w));
                    } else if on_stack.contains(w) {
                        let low_v = *lowlink.get(v).unwrap();
                        let idx_w = *indices.get(w).unwrap();
                        lowlink.insert(v, low_v.min(idx_w));
                    }
                }
            }

            if lowlink[v] == indices[v] {
                let mut component = Vec::new();
                while let Some(w) = stack.pop() {
                    on_stack.remove(w);
                    component.push(w.to_string());
                    if w == v {
                        break;
                    }
                }
                // Check if this component is truly recursive
                if component.len() > 1
                    || graph
                        .adj
                        .get(&component[0])
                        .map_or(false, |deps| deps.contains(&component[0]))
                {
                    result.push(component);
                }
            }
        }

        for v in self.adj.keys() {
            if !indices.contains_key(v.as_str()) {
                strongconnect(
                    v,
                    &mut index,
                    &mut indices,
                    &mut lowlink,
                    &mut stack,
                    &mut on_stack,
                    &mut result,
                    self,
                );
            }
        }
        result
    }

    pub fn recursive_nodes(&self) -> HashSet<String> {
        self.recursive_components().into_iter().flatten().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn edges(pairs: &[(&str, &str)]) -> RuleGraph {
        let mut g = RuleGraph::new();
        for (a, b) in pairs {
            g.add_edge((*a).into(), (*b).into());
        }
        g
    }

    #[test]
    fn test_no_cycle() {
        let g = edges(&[("Expr", "Atom"), ("Atom", "Number"), ("Number", "Digit")]);
        assert!(g.recursive_components().is_empty());
    }

    #[test]
    fn test_direct_self_cycle() {
        let g = edges(&[("Expr", "Expr")]);

        let comps = g.recursive_components();
        assert_eq!(comps.len(), 1);
        assert_eq!(comps[0], vec!["Expr".to_string()]);
    }

    #[test]
    fn test_mutual_cycle() {
        let g = edges(&[("Expr", "Atom"), ("Atom", "Expr")]);

        let comps = g.recursive_components();
        assert_eq!(comps.len(), 1);

        let mut comp = comps[0].clone();
        comp.sort();
        assert_eq!(comp, vec!["Atom".to_string(), "Expr".to_string()]);
    }

    #[test]
    fn test_indirect_cycle() {
        let g = edges(&[
            ("Expr", "Atom"),
            ("Atom", "MaybeParens"),
            ("MaybeParens", "Expr"),
        ]);

        let comps = g.recursive_components();
        assert_eq!(comps.len(), 1);

        let mut comp = comps[0].clone();
        comp.sort();
        assert_eq!(
            comp,
            vec![
                "Atom".to_string(),
                "Expr".to_string(),
                "MaybeParens".to_string()
            ]
        );
    }

    #[test]
    fn test_multiple_components() {
        let g = edges(&[
            ("Expr", "Expr"),   // self-cycle
            ("Atom", "Number"), // acyclic
            ("A", "B"),
            ("B", "C"),
            ("C", "A"), // A-B-C cycle
        ]);

        let mut comps = g.recursive_components();
        // Expect two recursive components: {"Expr"} and {"A","B","C"}
        comps.sort_by_key(|c| c.len());

        assert_eq!(comps.len(), 2);

        // The small one is just Expr
        assert_eq!(comps[0], vec!["Expr".to_string()]);

        // The larger one is the A-B-C cycle
        let mut abc = comps[1].clone();
        abc.sort();
        assert_eq!(abc, vec!["A".to_string(), "B".to_string(), "C".to_string()]);
    }
}
