use crate::{parser::Span, syntax::Type};

use super::{Context, Environment, Value};

#[derive(Clone, Debug)]
pub struct Prelude {
    types: Vec<(String, Type)>,
    context: Vec<(String, Type)>,
    environment: Vec<(String, Value)>,
}

impl Prelude {
    pub fn add_prelude_to(self, context: &mut Context, environment: &mut Environment) {
        self.types
            .into_iter()
            .for_each(|(name, r#type)| context.insert_type(name, r#type));
        self.context
            .into_iter()
            .for_each(|(name, r#type)| context.insert(name, r#type));
        self.environment
            .into_iter()
            .for_each(|(name, value)| environment.insert(name, value));
    }
}

impl Default for Prelude {
    fn default() -> Self {
        Self {
            types: make_type_prelude(),
            context: make_context_prelude(),
            environment: make_environment_prelude(),
        }
    }
}

fn make_type_prelude() -> Vec<(String, Type)> {
    vec![
        ("Bool".into(), Type::Bool(Span::default())),
        ("Int".into(), Type::Int(Span::default())),
    ]
}

fn make_context_prelude() -> Vec<(String, Type)> {
    vec![
        (
            "true".into(),
            Type::Variable(Span::default(), "Bool".into()),
        ),
        (
            "false".into(),
            Type::Variable(Span::default(), "Bool".into()),
        ),
    ]
}

fn make_environment_prelude() -> Vec<(String, Value)> {
    vec![]
}
