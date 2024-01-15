use super::{Pattern, Term, Type};

#[derive(Clone, Debug)]
pub struct Program(pub Vec<Declaration>);

#[derive(Clone, Debug)]
pub enum Declaration {
    Term(Pattern, Term),
    Type(String, Type),
}
