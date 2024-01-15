use std::{collections::HashMap, error::Error};

use crate::syntax::{Abstraction, Infix, Pattern, Prefix, Term, Type};

use super::Value;

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn eval(&mut self, term: &Term) -> Result<Value, Box<dyn Error>> {
        match term {
            Term::Abstraction(abstraction, _) => {
                Ok(Value::Abstraction(abstraction.clone(), self.clone()))
            }
            Term::Application(abstraction, argument, _) => {
                let argument = self.eval(argument)?;
                let Value::Abstraction(
                    Abstraction {
                        parameter,
                        parameter_type: _,
                        body,
                    },
                    environment,
                ) = self.eval(abstraction)?
                else {
                    return Err("Application expected abstraction in first position.".into());
                };
                let mut environment = environment.clone();
                environment.bind_pattern(&parameter, argument)?;
                environment.eval(&body.clone())
            }
            Term::Ascription(term, _, _) => self.eval(term),
            Term::Bool(b, _) => Ok(Value::Bool(*b)),
            Term::Fix(abstraction, span) => {
                let argument = Term::Abstraction(
                    Abstraction {
                        parameter: Pattern::Variable(span.clone(), "v".into()),
                        parameter_type: Type::Tuple(span.clone(), vec![]),
                        body: Box::new(Term::Application(
                            Box::new(term.clone()),
                            Box::new(Term::Variable("v".into(), span.clone())),
                            span.clone(),
                        )),
                    },
                    span.clone(),
                );
                let fixed =
                    Term::Application(abstraction.clone(), Box::new(argument), span.clone());
                self.eval(&fixed)
            }
            Term::If(condition, consequent, alternative, _) => {
                let Value::Bool(condition) = self.eval(condition)? else {
                    return Err("If expected boolean in condition.".into());
                };
                match condition {
                    true => self.eval(consequent),
                    false => self.eval(alternative),
                }
            }
            Term::Infix(left, op, right, _) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;
                match (left, op, right) {
                    (Value::Bool(l), Infix::Or, Value::Bool(r)) => Ok(Value::Bool(l || r)),
                    (Value::Bool(l), Infix::And, Value::Bool(r)) => Ok(Value::Bool(l && r)),
                    (left, Infix::Eq, right) => Ok(Value::Bool(left == right)),
                    (left, Infix::Ne, right) => Ok(Value::Bool(left != right)),
                    (Value::Int(l), Infix::Ge, Value::Int(r)) => Ok(Value::Bool(l >= r)),
                    (Value::Int(l), Infix::Gt, Value::Int(r)) => Ok(Value::Bool(l > r)),
                    (Value::Int(l), Infix::Le, Value::Int(r)) => Ok(Value::Bool(l <= r)),
                    (Value::Int(l), Infix::Lt, Value::Int(r)) => Ok(Value::Bool(l < r)),
                    (Value::Int(l), Infix::Add, Value::Int(r)) => Ok(Value::Int(l + r)),
                    (Value::Int(l), Infix::Sub, Value::Int(r)) => Ok(Value::Int(l - r)),
                    (Value::Int(l), Infix::Mul, Value::Int(r)) => Ok(Value::Int(l * r)),
                    (Value::Int(l), Infix::Div, Value::Int(r)) => Ok(Value::Int(l / r)),
                    (Value::Int(l), Infix::Rem, Value::Int(r)) => Ok(Value::Int(l % r)),
                    (Value::Int(l), Infix::Pow, Value::Int(r)) => {
                        Ok(Value::Int(l.pow(r.try_into().unwrap())))
                    }
                    _ => unimplemented!(),
                }
            }
            Term::Int(n, _) => Ok(Value::Int(*n)),
            Term::Let(pattern, value, body, _) => {
                let value = self.eval(value)?;
                let mut environment = self.clone();
                environment.bind_pattern(pattern, value)?;
                environment.eval(body)
            }
            Term::List(values, _) => values
                .iter()
                .map(|value| self.eval(value))
                .collect::<Result<_, _>>()
                .map(Value::List),
            Term::Match(value, arms, _) => {
                let Value::Variant { label, value } = self.eval(value)? else {
                    return Err("Match expected variant.".into());
                };
                let Some((pattern, body)) = arms.get(&label) else {
                    return Err(format!("Field {label} not in match arms.").into());
                };
                let mut environment = self.clone();
                environment.bind_pattern(pattern, *value)?;
                environment.eval(body)
            }
            Term::Postfix(_, _, _) => unimplemented!(),
            Term::Prefix(op, right, _) => {
                let right = self.eval(right)?;
                match (op, right) {
                    (Prefix::Neg, Value::Int(n)) => Ok(Value::Int(-n)),
                    (Prefix::Not, Value::Variant { label, value: _ }) => Ok(Value::Variant {
                        label: if label == "true" { "false" } else { "true" }.into(),
                        value: Box::new(Value::Tuple(vec![])),
                    }),
                    _ => unimplemented!(),
                }
            }
            Term::Record(fields, _) => fields
                .iter()
                .map(|(label, value)| self.eval(value).map(|value| (label.clone(), value)))
                .collect::<Result<_, _>>()
                .map(Value::Record),
            Term::RecordProjection(record, field, _) => {
                let record_value = self.eval(record)?;
                let Value::Record(fields) = record_value else {
                    return Err("Projection expected record.".into());
                };
                fields
                    .get(field)
                    .cloned()
                    .ok_or(format!("Field {} not in record.", field).into())
            }
            Term::Tuple(values, _) => values
                .iter()
                .map(|value| self.eval(value))
                .collect::<Result<_, _>>()
                .map(Value::Tuple),
            Term::TupleProjection(tuple, index, _) => {
                let Value::Tuple(values) = self.eval(tuple)? else {
                    return Err("Projection expected tuple.".into());
                };
                values
                    .get(*index)
                    .cloned()
                    .ok_or(format!("Index {} not in tuple.", index).into())
            }
            Term::Variable(name, _) => self.get(name).cloned(),
            Term::Variant(label, value, _) => {
                let value = self.eval(value)?;
                Ok(Value::variant(label, value))
            }
        }
    }

    pub fn bind_pattern(&mut self, pattern: &Pattern, value: Value) -> Result<(), Box<dyn Error>> {
        match (pattern, value) {
            (Pattern::Record(_, patterns), Value::Record(fields)) => {
                let extraneous: Vec<String> = patterns
                    .keys()
                    .filter(|field| !fields.contains_key(*field))
                    .map(String::clone)
                    .collect();
                if !extraneous.is_empty() {
                    return Err(format!("Fields {} not in record.", extraneous.join(", ")).into());
                }
                for (field, pattern) in patterns {
                    let value = fields.get(field).unwrap().clone();
                    self.bind_pattern(pattern, value)?;
                }
            }
            (Pattern::Record(_, _), _) => {
                return Err("Record pattern incompatible with value.".into());
            }
            (Pattern::Tuple(_, patterns), Value::Tuple(values)) => {
                if patterns.len() != values.len() {
                    return Err("Tuple pattern insufficient.".into());
                }
                for (pattern, value) in patterns.iter().zip(values.into_iter()) {
                    self.bind_pattern(pattern, value)?;
                }
            }
            (Pattern::Tuple(_, _), _) => {
                return Err("Tuple pattern incompatible with value.".into());
            }
            (Pattern::Variable(_, name), value) => {
                self.insert(name.clone(), value);
            }
            (Pattern::Wildcard(_), _) => {}
        }
        Ok(())
    }

    pub fn get(&self, name: &String) -> Result<&Value, Box<dyn Error>> {
        self.values
            .get(name)
            .ok_or(format!("Variable {name} not in environment.").into())
    }

    pub fn insert(&mut self, k: String, v: Value) {
        self.values.insert(k, v);
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            values: HashMap::from([
                ("true".into(), Value::Bool(true)),
                ("false".into(), Value::Bool(false)),
            ]),
        }
    }
}
