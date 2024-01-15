use std::collections::HashMap;

use ariadne::Span as AriadneSpan;
use indexmap::{indexmap, IndexMap};

use crate::{
    error::{PatternError, Reporable, TypeError},
    parser::Span,
    syntax::{Abstraction, Infix, Pattern, Prefix, Term, Type},
};

type Errors = Vec<Box<dyn Reporable>>;

#[derive(Clone, Default, Debug)]
pub struct Context {
    context: HashMap<String, Type>,
    values: HashMap<String, Type>,
}

impl Context {
    pub fn type_of(&mut self, term: &Term) -> Result<Type, Errors> {
        match term {
            Term::Abstraction(
                Abstraction {
                    parameter,
                    parameter_type,
                    body,
                },
                span,
            ) => {
                let mut context = self.clone();
                let parameter_type = self.resolve(parameter_type.clone())?;
                context.bind_pattern(parameter, term, parameter_type.clone())?;
                let body_type = context.resolve_type_of(body)?;
                Ok(Type::Abstraction(
                    span.clone(),
                    Box::new(parameter_type),
                    Box::new(body_type),
                ))
            }
            Term::Application(abstraction, argument, _) => {
                let abstraction_type = self.resolve_type_of(abstraction)?;
                let argument_type = self.resolve_type_of(argument)?;
                match abstraction_type {
                    Type::Abstraction(_, parameter_type, return_type) => {
                        let parameter_type = self.resolve(*parameter_type)?;
                        let return_type = self.resolve(*return_type)?;
                        match parameter_type == argument_type {
                            true => Ok(return_type.clone()),
                            false => TypeError::Mismatch {
                                offset: term.span().start(),
                                span: argument.span().clone(),
                                actual: parameter_type,
                                expected: argument_type,
                            }
                            .into(),
                        }
                    }
                    _ => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: abstraction.span().clone(),
                        actual: abstraction_type,
                        expected: Type::Abstraction(
                            Span::default(),
                            Box::new(Type::Variable(Span::default(), "*".into())),
                            Box::new(Type::Variable(Span::default(), "*".into())),
                        ),
                    }
                    .into(),
                }
            }
            Term::Ascription(value, as_type, _) => {
                let as_type = self.resolve(as_type.clone())?;
                let value_type = self.resolve_type_of(value)?;
                if value_type == as_type {
                    return Ok(value_type);
                }
                match (&value_type, &as_type) {
                    (Type::Variant(_, term_variants), Type::Variant(_, variants))
                        if term_variants
                            .iter()
                            .all(|(label, r#type)| variants.get(label) == Some(r#type)) =>
                    {
                        Ok(as_type)
                    }
                    (Type::List(_, None), Type::List(_, Some(_))) => Ok(as_type),
                    (_, _) if value_type == as_type => Ok(value_type),
                    _ => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: value.span().clone(),
                        actual: value_type,
                        expected: as_type,
                    }
                    .into(),
                }
            }
            Term::Bool(_, span) => Ok(Type::Bool(span.clone())),
            Term::Fix(abstraction, _) => {
                let abstraction_type = self.resolve_type_of(abstraction)?;
                let Type::Abstraction(_, parameter_type, return_type) = abstraction_type else {
                    return TypeError::Mismatch {
                        offset: term.span().start(),
                        span: abstraction.span().clone(),
                        actual: abstraction_type,
                        expected: Type::Abstraction(
                            Span::default(),
                            Box::new(Type::Variable(Span::default(), "*".into())),
                            Box::new(Type::Variable(Span::default(), "*".into())),
                        ),
                    }
                    .into();
                };
                let parameter_type = self.resolve(*parameter_type)?;
                let return_type = self.resolve(*return_type)?;
                match parameter_type == return_type {
                    true => Ok(return_type),
                    false => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: parameter_type.span().clone(),
                        actual: parameter_type,
                        expected: return_type,
                    }
                    .into(),
                }
            }
            Term::If(condition, consequent, alternative, _) => {
                let condition_type = self.resolve_type_of(condition)?;
                if !condition_type.is_bool() {
                    return TypeError::Mismatch {
                        offset: term.span().start(),
                        span: condition.span().clone(),
                        actual: condition_type,
                        expected: Type::Bool(Span::default()),
                    }
                    .into();
                }
                let consequent_type = self.resolve_type_of(consequent)?;
                let alternative_type = self.resolve_type_of(alternative)?;
                match consequent_type == alternative_type {
                    true => Ok(consequent_type),
                    false => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: alternative.span().clone(),
                        actual: alternative_type,
                        expected: consequent_type,
                    }
                    .into(),
                }
            }
            Term::Infix(left, op, right, span) => {
                let left_type = self.resolve_type_of(left)?;
                let right_type = self.resolve_type_of(right)?;
                match op {
                    Infix::Or | Infix::And => {
                        let mut errors: Errors = vec![];
                        if !left_type.is_bool() {
                            errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: left.span().clone(),
                                    actual: left_type,
                                    expected: Type::Bool(Span::default()),
                                }
                                .into(),
                            );
                        }
                        if !right_type.is_bool() {
                            errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: right.span().clone(),
                                    actual: right_type,
                                    expected: Type::Bool(Span::default()),
                                }
                                .into(),
                            );
                        }
                        match errors.is_empty() {
                            true => Ok(Type::Bool(span.clone())),
                            false => Err(errors),
                        }
                    }
                    Infix::Eq | Infix::Ne => match right_type == left_type {
                        true => Ok(Type::Bool(span.clone())),
                        false => TypeError::Mismatch {
                            offset: term.span().start(),
                            span: right.span().clone(),
                            actual: right_type,
                            expected: left_type,
                        }
                        .into(),
                    },
                    Infix::Ge | Infix::Gt | Infix::Le | Infix::Lt => {
                        let mut errors: Errors = vec![];
                        if !left_type.is_int() {
                            errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: left.span().clone(),
                                    actual: left_type,
                                    expected: Type::Int(Span::default()),
                                }
                                .into(),
                            );
                        }
                        if !right_type.is_int() {
                            errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: right.span().clone(),
                                    actual: right_type,
                                    expected: Type::Int(Span::default()),
                                }
                                .into(),
                            );
                        }
                        match errors.is_empty() {
                            true => Ok(Type::Bool(span.clone())),
                            false => Err(errors),
                        }
                    }
                    Infix::Add | Infix::Sub | Infix::Mul | Infix::Div | Infix::Rem | Infix::Pow => {
                        let mut errors: Errors = vec![];
                        if !left_type.is_int() {
                            errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: left.span().clone(),
                                    actual: left_type.clone(),
                                    expected: Type::Int(Span::default()),
                                }
                                .into(),
                            );
                        }
                        if !right_type.is_int() {
                            errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: left.span().clone(),
                                    actual: left_type.clone(),
                                    expected: Type::Int(Span::default()),
                                }
                                .into(),
                            );
                        }
                        match errors.is_empty() {
                            true => Ok(Type::Int(span.clone())),
                            false => Err(errors),
                        }
                    }
                }
            }
            Term::Int(_, span) => Ok(Type::Int(span.clone())),
            Term::Let(pattern, value, body, _) => {
                let value_type = self.resolve_type_of(value)?;
                let mut context = self.clone();
                context.bind_pattern(pattern, term, value_type)?;
                context.resolve_type_of(body)
            }
            Term::List(values, span) => match values.is_empty() {
                true => Ok(Type::List(span.clone(), None)),
                false => {
                    let values_type = self.resolve_type_of(values.first().unwrap())?;
                    let mut errors = vec![];
                    for value in values.iter().skip(1) {
                        match self.resolve_type_of(value) {
                            Ok(value_type) if value_type != values_type => errors.push(
                                TypeError::Mismatch {
                                    offset: term.span().start(),
                                    span: value.span().clone(),
                                    actual: value_type,
                                    expected: values_type.clone(),
                                }
                                .into(),
                            ),
                            Ok(_) => {}
                            Err(mut err) => errors.append(&mut err),
                        }
                    }
                    match errors.is_empty() {
                        true => Ok(Type::List(span.clone(), Some(Box::new(values_type)))),
                        false => Err(errors),
                    }
                }
            },
            Term::Match(value, arms, _) => {
                let value_type = self.resolve_type_of(value)?;
                let Type::Variant(span, variants) = &value_type else {
                    return TypeError::Mismatch {
                        offset: term.span().start(),
                        span: value.span().clone(),
                        actual: value_type,
                        expected: Type::Variant(
                            Span::default(),
                            indexmap! {
                                "...".into() => Type::Variable(Span::default(), "*".into())
                            },
                        ),
                    }
                    .into();
                };
                let absent: Vec<(String, Type)> = variants
                    .iter()
                    .filter_map(|(label, variant_type)| {
                        (!arms.contains_key(label)).then_some((label.clone(), variant_type.clone()))
                    })
                    .collect();
                let extraneous: Vec<String> = arms
                    .iter()
                    .filter_map(|(label, _)| {
                        (!variants.contains_key(label)).then_some(label.clone())
                    })
                    .collect();
                let mut errors: Errors = vec![];
                if !absent.is_empty() {
                    errors.push(
                        TypeError::MissingVariants(term.span().start(), span.clone(), absent)
                            .into(),
                    );
                }
                if !extraneous.is_empty() {
                    todo!() // TODO: Make warning
                }
                if !errors.is_empty() {
                    return Err(errors);
                }
                let mut arm_types = IndexMap::new();
                for (label, (pattern, body)) in arms {
                    let mut context = self.clone();
                    let label_type = variants.get(label).unwrap().clone();
                    if let Err(mut err) = context.bind_pattern(pattern, term, label_type) {
                        errors.append(&mut err);
                    }
                    match context.resolve_type_of(body) {
                        Ok(arm_type) => {
                            arm_types.insert(label.clone(), (arm_type, body.span().clone()));
                        }
                        Err(mut es) => errors.append(&mut es),
                    }
                }
                if !errors.is_empty() {
                    return Err(errors);
                }
                let (match_type, _) = arm_types.first().unwrap().1.clone();
                for (_, (arm_type, span)) in arm_types {
                    if arm_type != match_type {
                        errors.push(
                            TypeError::Mismatch {
                                offset: term.span().start(),
                                span,
                                actual: arm_type,
                                expected: match_type.clone(),
                            }
                            .into(),
                        );
                    }
                }
                match errors.is_empty() {
                    true => Ok(match_type),
                    false => Err(errors),
                }
            }
            Term::Postfix(_, _, _) => unimplemented!(),
            Term::Prefix(op, right, span) => {
                let right_type = self.resolve_type_of(right)?;
                match (op, right_type) {
                    (Prefix::Neg, Type::Int(_)) => Ok(Type::Int(span.clone())),
                    (Prefix::Neg, right_type) => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: right.span().clone(),
                        actual: right_type,
                        expected: Type::Int(Span::default()),
                    }
                    .into(),
                    (Prefix::Not, Type::Bool(_)) => Ok(Type::Bool(span.clone())),
                    (Prefix::Not, right_type) => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: right.span().clone(),
                        actual: right_type,
                        expected: Type::Bool(Span::default()),
                    }
                    .into(),
                }
            }
            Term::Record(fields, span) => fields
                .iter()
                .map(|(field, value)| {
                    self.resolve_type_of(value)
                        .map(|value_type| (field.clone(), value_type))
                })
                .collect::<Result<_, _>>()
                .map(|fields| Type::Record(span.clone(), fields)),
            Term::RecordProjection(record, label, span) => {
                let raw_type = self.type_of(record)?;
                let record_type = self.resolve(raw_type.clone())?;
                match &record_type {
                    Type::Record(_, fields) => fields.get(label).cloned().ok_or(
                        TypeError::UnknownField(
                            term.span().start(),
                            span.clone(),
                            label.clone(),
                            raw_type,
                        )
                        .into(),
                    ),
                    _ => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: record.span().clone(),
                        actual: record_type.clone(),
                        expected: Type::Record(
                            Span::default(),
                            indexmap! {
                                "...".into() => Type::Variable(Span::default(), "*".into())
                            },
                        ),
                    }
                    .into(),
                }
            }
            Term::Tuple(values, span) => values
                .iter()
                .map(|value| self.resolve_type_of(value))
                .collect::<Result<_, _>>()
                .map(|value_types| Type::Tuple(span.clone(), value_types)),
            Term::TupleProjection(tuple, index, span) => {
                let raw_type = self.type_of(tuple)?;
                let tuple_type = self.resolve(raw_type.clone())?;
                match tuple_type {
                    Type::Tuple(_, value) => value.get(*index).cloned().ok_or(
                        TypeError::UnknownIndex {
                            offset: term.span().start(),
                            span: span.clone(),
                            index: *index,
                            tuple_type: raw_type,
                        }
                        .into(),
                    ),
                    _ => TypeError::Mismatch {
                        offset: term.span().start(),
                        span: tuple.span().clone(),
                        actual: tuple_type.clone(),
                        expected: Type::Tuple(
                            Span::default(),
                            vec![Type::Variable(Span::default(), "...*".into())],
                        ),
                    }
                    .into(),
                }
            }
            Term::Variable(name, span) => self.get(name).cloned().ok_or(
                TypeError::UndefinedVariable(term.span().start(), span.clone(), name.clone())
                    .into(),
            ),
            Term::Variant(label, value, span) => {
                let value_type = self.resolve_type_of(value)?;
                Ok(Type::Variant(
                    span.clone(),
                    IndexMap::from([(label.clone(), value_type)]),
                ))
            }
        }
    }

    pub fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        _term: &Term,
        raw_type: Type,
    ) -> Result<(), Errors> {
        let r#type = self.resolve(raw_type.clone())?;
        match (pattern, r#type) {
            (Pattern::Record(span, patterns), Type::Record(_, fields)) => {
                let extraneous: Vec<String> = patterns
                    .keys()
                    .filter(|field| !fields.contains_key(*field))
                    .map(String::clone)
                    .collect();
                if !extraneous.is_empty() {
                    return PatternError::UnknownFields(span.clone(), extraneous, raw_type).into();
                }
                for (field, pattern) in patterns {
                    let value_type = fields.get(field).unwrap().clone();
                    self.bind_pattern(pattern, _term, value_type)?;
                }
            }
            (Pattern::Record(span, _), other_type) => {
                return PatternError::Incompatible {
                    span: span.clone(),
                    actual: pattern.clone(),
                    expected: other_type,
                }
                .into();
            }
            (Pattern::Tuple(span, patterns), Type::Tuple(_, types)) => {
                if patterns.len() != types.len() {
                    return PatternError::MissingElements {
                        span: span.clone(),
                        actual: patterns.len(),
                        expected: types.len(),
                    }
                    .into();
                }
                for (pattern, r#type) in patterns.iter().zip(types.into_iter()) {
                    self.bind_pattern(pattern, _term, r#type)?;
                }
            }
            (Pattern::Tuple(span, _), other_type) => {
                return PatternError::Incompatible {
                    span: span.clone(),
                    actual: pattern.clone(),
                    expected: other_type,
                }
                .into();
            }
            (Pattern::Variable(_, name), value) => {
                self.insert(name.clone(), value);
            }
            (Pattern::Wildcard(_), _) => {}
        }
        Ok(())
    }

    pub fn get(&self, name: &String) -> Option<&Type> {
        self.values.get(name)
    }

    pub fn insert<S: Into<String>>(&mut self, name: S, r#type: Type) {
        self.values.insert(name.into(), r#type);
    }

    pub fn insert_type<S: Into<String>>(&mut self, name: S, r#type: Type) {
        self.context.insert(name.into(), r#type);
    }

    pub fn resolve(&self, r#type: Type) -> Result<Type, Errors> {
        match r#type {
            Type::Abstraction(span, parameter_type, return_type) => {
                let parameter_type = self.resolve(*parameter_type)?;
                let return_type = self.resolve(*return_type)?;
                Ok(Type::Abstraction(
                    span.clone(),
                    Box::new(parameter_type),
                    Box::new(return_type),
                ))
            }
            Type::List(span, Some(values_type)) => self
                .resolve(*values_type)
                .map(|value_type| Type::List(span.clone(), Some(Box::new(value_type)))),
            Type::Record(span, fields) => {
                let mut field_types = indexmap! {};
                let mut errors = vec![];
                for (label, value_type) in fields {
                    match self.resolve(value_type) {
                        Ok(value_type) => drop(field_types.insert(label, value_type)),
                        Err(mut err) => errors.append(&mut err),
                    }
                }
                match errors.is_empty() {
                    true => Ok(Type::Record(span.clone(), field_types)),
                    false => Err(errors),
                }
            }
            Type::Tuple(span, values) => {
                let mut value_types = vec![];
                let mut errors = vec![];
                for value_type in values {
                    match self.resolve(value_type) {
                        Ok(value_type) => value_types.push(value_type),
                        Err(mut err) => errors.append(&mut err),
                    }
                }
                match errors.is_empty() {
                    true => Ok(Type::Tuple(span.clone(), value_types)),
                    false => Err(errors),
                }
            }
            Type::Variable(span, name) => self
                .context
                .get(&name)
                .cloned()
                .ok_or(TypeError::UnknownType(span.start(), span.clone(), name).into()),
            Type::Variant(span, variants) => {
                let mut variant_types = indexmap! {};
                let mut errors = vec![];
                for (label, variant_type) in variants {
                    match self.resolve(variant_type) {
                        Ok(variant_type) => drop(variant_types.insert(label, variant_type)),
                        Err(mut err) => errors.append(&mut err),
                    }
                }
                match errors.is_empty() {
                    true => Ok(Type::Variant(span, variant_types)),
                    false => Err(errors),
                }
            }
            _ => Ok(r#type),
        }
    }

    pub fn resolve_type_of(&mut self, term: &Term) -> Result<Type, Errors> {
        let r#type = self.type_of(term)?;
        self.resolve(r#type)
    }
}
