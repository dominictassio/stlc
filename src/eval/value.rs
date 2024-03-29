use std::{collections::HashMap, fmt::Display};

use crate::syntax::Abstraction;

use super::Environment;

#[derive(Clone, Debug)]
pub enum Value {
    Abstraction(Abstraction, Environment),
    Bool(bool),
    Int(i64),
    List(Vec<Value>),
    Record(HashMap<String, Value>),
    Tuple(Vec<Value>),
    Variant { label: String, value: Box<Value> },
}

impl Value {
    pub fn variant(label: &str, value: Value) -> Self {
        Value::Variant {
            label: label.to_string(),
            value: Box::new(value),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Record(l0), Self::Record(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (
                Self::Variant {
                    label: l_label,
                    value: l_value,
                },
                Self::Variant {
                    label: r_label,
                    value: r_value,
                },
            ) => l_label == r_label && l_value == r_value,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Abstraction(abstraction, _) => {
                write!(f, "{abstraction}")
            }
            Value::Bool(b) => write!(f, "{b}"),
            Value::Int(n) => write!(f, "{n}"),
            Value::List(values) => {
                let values: Vec<String> = values.iter().map(Value::to_string).collect();
                write!(f, "[{}]", values.join(", "))
            }
            Value::Record(fields) => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|(field, value)| format!("{field} = {value}"))
                    .collect();
                write!(f, "{{ {} }}", fields.join(", "))
            }
            Value::Tuple(values) => {
                let values: Vec<String> = values.iter().map(Value::to_string).collect();
                write!(f, "({})", values.join(", "))
            }
            Value::Variant { label, value } => write!(f, "<{label} = {value}>"),
        }
    }
}
