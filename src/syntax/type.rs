use std::fmt::Display;

use indexmap::IndexMap;

use crate::parser::Span;

#[derive(Clone, Debug)]
pub enum Type {
    Abstraction(Span, Box<Type>, Box<Type>),
    Bool(Span),
    Int(Span),
    List(Span, Option<Box<Type>>),
    Record(Span, IndexMap<String, Type>),
    Tuple(Span, Vec<Type>),
    Variable(Span, String),
    Variant(Span, IndexMap<String, Type>),
}

impl Type {
    pub fn span(&self) -> &Span {
        match self {
            Type::Abstraction(span, _, _) => span,
            Type::Bool(span) => span,
            Type::Int(span) => span,
            Type::List(span, _) => span,
            Type::Record(span, _) => span,
            Type::Tuple(span, _) => span,
            Type::Variable(span, _) => span,
            Type::Variant(span, _) => span,
        }
    }

    /// Returns `true` if the type is [`Bool`].
    ///
    /// [`Bool`]: Type::Bool
    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }

    /// Returns `true` if the type is [`Int`].
    ///
    /// [`Int`]: Type::Int
    #[must_use]
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    /// Returns `true` if the type is [`Tuple`].
    ///
    /// [`Tuple`]: Type::Tuple
    #[must_use]
    pub fn is_tuple(&self) -> bool {
        matches!(self, Self::Tuple(..))
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Abstraction(_, l1, l2), Self::Abstraction(_, r1, r2)) => l1 == r1 && l2 == r2,
            (Self::Bool(_), Self::Bool(_)) => true,
            (Self::Int(_), Self::Int(_)) => true,
            (Self::List(_, l1), Self::List(_, r1)) => l1 == r1,
            (Self::Record(_, l1), Self::Record(_, r1)) => l1 == r1,
            (Self::Tuple(_, l1), Self::Tuple(_, r1)) => l1 == r1,
            (Self::Variable(_, l1), Self::Variable(_, r1)) => l1 == r1,
            (Self::Variant(_, l1), Self::Variant(_, r1)) => l1 == r1,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Abstraction(_, parameter_type, return_type) => {
                match **parameter_type {
                    Type::Abstraction(_, _, _) => write!(f, "({parameter_type})")?,
                    _ => write!(f, "{parameter_type}")?,
                }
                write!(f, " -> {return_type}")
            }
            Type::Bool(_) => write!(f, "Bool"),
            Type::Int(_) => write!(f, "Int"),
            Type::List(_, Some(values_type)) => {
                write!(f, "[")?;
                match **values_type {
                    Type::Abstraction(_, _, _) => write!(f, "({values_type})")?,
                    _ => write!(f, "{values_type}")?,
                }
                write!(f, "]")
            }
            Type::List(_, None) => write!(f, "[]"),
            Type::Record(_, fields) => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|(label, field_type)| format!("{label}: {field_type}"))
                    .collect();
                write!(f, "{{ {} }}", fields.join(", "))
            }
            Type::Tuple(_, types) => {
                let types: Vec<String> = types.iter().map(Type::to_string).collect();
                write!(f, "({})", types.join(", "))
            }
            Type::Variable(_, name) => write!(f, "{name}"),
            Type::Variant(_, variants) => {
                let variants: Vec<String> = variants
                    .iter()
                    .map(|(label, variant_type)| format!("{label}: {variant_type}"))
                    .collect();
                write!(f, "<{}>", variants.join(", "))
            }
        }
    }
}
