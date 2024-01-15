use std::{collections::HashMap, fmt::Display};

use crate::parser::Span;

#[derive(Clone, Debug)]
pub enum Pattern {
    Record(Span, HashMap<String, Pattern>),
    Tuple(Span, Vec<Pattern>),
    Variable(Span, String),
    Wildcard(Span),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Record(_, fields) => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|(name, pattern)| format!("{name} = {pattern}"))
                    .collect();
                write!(f, "{{ {} }}", fields.join(", "))
            }
            Pattern::Tuple(_, patterns) => {
                let patterns: Vec<String> = patterns.iter().map(Pattern::to_string).collect();
                write!(f, "({})", patterns.join(", "))
            }
            Pattern::Variable(_, name) => write!(f, "{name}"),
            Pattern::Wildcard(_) => write!(f, "_"),
        }
    }
}
