pub use regex_syntax::hir::{Class, Literal};
use regex_syntax::{
    ParserBuilder,
    hir::{Hir, HirKind, Look},
};
use snafu::{ResultExt, Snafu};

#[derive(Debug, Snafu)]
pub enum MirError {
    #[snafu(display("todo {:#?}", source))]
    FromHir { source: regex_syntax::Error },
    #[snafu(display("todo {:#?}", look))]
    Look { look: Look },
}

#[derive(Clone, Debug, PartialEq, Eq)]

pub enum Mir {
    /// “ε” – matches the empty string
    Empty,
    /// A literal
    Literal(Literal),
    /// A character‐class, e.g. [A-Z], [0-9], etc.
    CharClass(Class),
    /// Sequence (concatenation) of sub-patterns
    Sequence(Vec<Mir>),
    /// Alternation (choice) between branches, e.g. a | b | c
    Choice(Vec<Mir>),
    /// Zero-or-more repetition (the `*` quantifier)
    ZeroOrMore(Box<Mir>),
    /// Optional (zero-or-one, the `?` quantifier)
    Optional(Box<Mir>),
}

impl Mir {
    pub fn parse(input: &str) -> Result<Self, MirError> {
        let hir = ParserBuilder::new()
            .unicode(true)
            .build()
            .parse(input)
            .context(FromHirSnafu)?;
        Mir::try_from(hir)
    }
}

impl TryFrom<Hir> for Mir {
    type Error = MirError;

    fn try_from(hir: Hir) -> Result<Self, MirError> {
        match hir.into_kind() {
            HirKind::Empty => Ok(Mir::Empty),
            HirKind::Concat(concat) => {
                // flatten nested Sequence nodes
                let mut out = Vec::with_capacity(concat.len());
                for hir in concat {
                    match Mir::try_from(hir)? {
                        Mir::Sequence(nested) => out.extend(nested),
                        other => out.push(other),
                    }
                }

                Ok(Mir::Sequence(out))
            }
            HirKind::Alternation(alts) => {
                // flatten nested Choice nodes
                let mut out = Vec::with_capacity(alts.len());
                for hir in alts {
                    match Mir::try_from(hir)? {
                        Mir::Choice(nested) => out.extend(nested),
                        other => out.push(other),
                    }
                }
                Ok(Mir::Choice(out))
            }
            HirKind::Literal(lit) => Ok(Mir::Literal(lit)),
            HirKind::Class(class) => Ok(Mir::CharClass(class)),
            HirKind::Repetition(rep) => {
                let sub = Mir::try_from(*rep.sub)?;
                let min = rep.min;
                // if max is None, treat as unbounded
                let max = rep.max.unwrap_or(rep.min);

                // first, exactly `min` copies
                let mut parts = Vec::with_capacity((max - min) as usize + 1);
                for _ in 0..min {
                    parts.push(sub.clone());
                }

                if rep.max.is_none() {
                    // {min,}  =>  after the `min`, zero-or-more
                    parts.push(Mir::ZeroOrMore(Box::new(sub)));
                } else if max > min {
                    // {min,max} => exactly `min`, then (max-min) optionals
                    for _ in min..max {
                        parts.push(Mir::Optional(Box::new(sub.clone())));
                    }
                }
                // if max == min, we’re done (exactly min)

                // collapse singletons
                Ok(if parts.len() == 1 {
                    parts.into_iter().next().unwrap()
                } else {
                    Mir::Sequence(parts)
                })
            }
            HirKind::Capture(capture) => Mir::try_from(*capture.sub),
            HirKind::Look(look) => LookSnafu { look }.fail(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn equivalent_patterns() {
        let inputs = [
            ("a|b", "[a-b]"),
            ("1|2|3", "[1-3]"),
            ("1+", "[1]+"),
            ("c*", "[c]*"),
            ("a{3}", "a{3}"),
            ("a[a]{2}", "a{3}"),
        ];

        for (left, right) in inputs.iter() {
            let mir_left = Mir::parse(left).unwrap();
            let mir_right = Mir::parse(right).unwrap();
            assert_eq!(
                mir_left, mir_right,
                "Regexes \"{left}\" and \"{right}\" are different"
            );
        }
    }
}
