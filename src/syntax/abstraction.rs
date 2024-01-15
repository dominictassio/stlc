use std::fmt::Display;

use super::{Pattern, Term, Type};

#[derive(Clone, Debug)]
pub struct Abstraction {
    pub parameter: Pattern,
    pub parameter_type: Type,
    pub body: Box<Term>,
}

impl Display for Abstraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Abstraction {
            parameter,
            parameter_type,
            body,
        } = self;
        write!(f, "{parameter}: {parameter_type} => {body}")
    }
}
