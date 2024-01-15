use std::fmt::Display;

use indexmap::IndexMap;

use crate::parser::Span;

use super::{Abstraction, Pattern, Type};

#[derive(Clone, Debug)]
pub enum Term {
    Abstraction(Abstraction, Span),
    Application(Box<Term>, Box<Term>, Span),
    Ascription(Box<Term>, Type, Span),
    #[allow(dead_code)]
    Bool(bool, Span),
    Fix(Box<Term>, Span),
    If(Box<Term>, Box<Term>, Box<Term>, Span),
    Infix(Box<Term>, Infix, Box<Term>, Span),
    Int(i64, Span),
    Let(Pattern, Box<Term>, Box<Term>, Span),
    List(Vec<Term>, Span),
    Match(Box<Term>, IndexMap<String, (Pattern, Term)>, Span),
    #[allow(dead_code)]
    Postfix(Box<Term>, Postfix, Span),
    Prefix(Prefix, Box<Term>, Span),
    Record(IndexMap<String, Term>, Span),
    RecordProjection(Box<Term>, String, Span),
    Tuple(Vec<Term>, Span),
    TupleProjection(Box<Term>, usize, Span),
    Variable(String, Span),
    Variant(String, Box<Term>, Span),
}

impl Term {
    pub fn span(&self) -> &Span {
        match self {
            Term::Abstraction(_, span) => span,
            Term::Application(_, _, span) => span,
            Term::Ascription(_, _, span) => span,
            Term::Bool(_, span) => span,
            Term::Fix(_, span) => span,
            Term::If(_, _, _, span) => span,
            Term::Infix(_, _, _, span) => span,
            Term::Int(_, span) => span,
            Term::Let(_, _, _, span) => span,
            Term::List(_, span) => span,
            Term::Match(_, _, span) => span,
            Term::Postfix(_, _, span) => span,
            Term::Prefix(_, _, span) => span,
            Term::Record(_, span) => span,
            Term::RecordProjection(_, _, span) => span,
            Term::Tuple(_, span) => span,
            Term::TupleProjection(_, _, span) => span,
            Term::Variable(_, span) => span,
            Term::Variant(_, _, span) => span,
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Abstraction(abstraction, _) => write!(f, "{abstraction}"),
            Term::Application(abstraction, argument, _) => write!(f, "{abstraction} {argument}"),
            Term::Ascription(value, as_type, _) => write!(f, "{value} as {as_type}"),
            Term::Bool(b, _) => write!(f, "{b}"),
            Term::Fix(value, _) => write!(f, "fix {value}"),
            Term::If(condition, consequent, alternative, _) => {
                write!(f, "if {condition} then {consequent} else {alternative}")
            }
            Term::Infix(left, op, right, _) => write!(f, "{left} {op} {right}"),
            Term::Int(n, _) => write!(f, "{n}"),
            Term::Let(pattern, value, body, _) => write!(f, "let {pattern} = {value} in {body}"),
            Term::List(values, _) => {
                let values: Vec<String> = values.iter().map(Term::to_string).collect();
                write!(f, "[{}]", values.join(", "))
            }
            Term::Match(value, arms, _) => {
                let arms: Vec<String> = arms
                    .iter()
                    .map(|(tag, (pattern, body))| format!("<{tag}={pattern}> => {body}"))
                    .collect();
                write!(f, "match {value} with {}", arms.join(", "))
            }
            Term::Postfix(left, op, _) => write!(f, "{left}{op}"),
            Term::Prefix(op, right, _) => write!(f, "{op}{right}"),
            Term::Record(fields, _) => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|(name, value)| format!("{name} = {value}"))
                    .collect();
                write!(f, "{{ {} }}", fields.join(", "))
            }
            Term::RecordProjection(record, label, _) => write!(f, "{record}.{label}"),
            Term::Tuple(values, _) => {
                let values: Vec<String> = values.iter().map(Term::to_string).collect();
                write!(f, "({})", values.join(", "))
            }
            Term::TupleProjection(tuple, index, _) => write!(f, "{tuple}.{index}"),
            Term::Variable(name, _) => write!(f, "{name}"),
            Term::Variant(field, value, _) => write!(f, "<{field} = {value}>"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Infix {
    Or,
    And,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            Infix::Or => "||",
            Infix::And => "&&",
            Infix::Eq => "==",
            Infix::Ne => "!=",
            Infix::Ge => ">=",
            Infix::Gt => ">",
            Infix::Le => "<=",
            Infix::Lt => "<",
            Infix::Add => "+",
            Infix::Sub => "-",
            Infix::Mul => "*",
            Infix::Div => "/",
            Infix::Rem => "%",
            Infix::Pow => "**",
        };
        write!(f, "{op}")
    }
}

#[derive(Clone, Debug)]
pub enum Postfix {}

impl Display for Postfix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Clone, Debug)]
pub enum Prefix {
    Neg,
    Not,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            Prefix::Neg => "-",
            Prefix::Not => "!",
        };
        write!(f, "{op}")
    }
}
