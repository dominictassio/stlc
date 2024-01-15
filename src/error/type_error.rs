use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Span as AriadneSpan};

use crate::{parser::Span, syntax::Type};

use super::Reporable;

#[derive(Clone, Debug)]
pub enum TypeError {
    Mismatch {
        offset: usize,
        span: Span,
        actual: Type,
        expected: Type,
    },
    MissingVariants(usize, Span, Vec<(String, Type)>),
    UndefinedVariable(usize, Span, String),
    UnknownField(usize, Span, String, Type),
    UnknownIndex {
        offset: usize,
        span: Span,
        index: usize,
        tuple_type: Type,
    },
    UnknownType(usize, Span, String),
}

impl Reporable for TypeError {
    fn build_report(&self) -> Report<Span> {
        let report = Report::<Span>::build(ReportKind::Error, self.span().source(), self.offset());
        match self {
            TypeError::Mismatch {
                offset: _,
                span,
                actual,
                expected,
            } => report.with_message("mismatched types").with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "expected `{}`, found `{}`",
                        expected.to_string().fg(Color::Green),
                        actual.to_string().fg(Color::Red)
                    ))
                    .with_color(Color::Red),
            ),
            TypeError::MissingVariants(_, span, variants) => report
                .with_message("missing variants")
                .with_labels(variants.iter().map(|(label, variant_type)| {
                    Label::new(span.clone())
                        .with_message(format!(
                            "missing {}{} {}",
                            label.to_string().fg(Color::Red),
                            "{}".fg(Color::Red),
                            variant_type.to_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red)
                }))
                .with_note("Ensure all variants are exhausted by match arms."),
            TypeError::UndefinedVariable(_, span, name) => report
                .with_message(format!("cannot find variable `{name}` in this scope"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                ),
            TypeError::UnknownField(_, span, label, field_type) => report
                .with_message(format!("no field `{label}` on record `{field_type}`"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("unknown field")
                        .with_color(Color::Red),
                ),
            TypeError::UnknownIndex {
                offset: _,
                span,
                index,
                tuple_type,
            } => report
                .with_message(format!("no index `{index}` in tuple `{tuple_type}`"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("unknown index")
                        .with_color(Color::Red),
                ),
            TypeError::UnknownType(_, span, name) => report
                .with_message(format!("cannot find type `{name}` in this scope"))
                .with_label(
                    Label::new(span.clone())
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                ),
        }
        .finish()
    }

    fn offset(&self) -> usize {
        match self {
            TypeError::Mismatch {
                offset,
                span: _,
                actual: _,
                expected: _,
            } => *offset,
            TypeError::MissingVariants(offset, _, _) => *offset,
            TypeError::UndefinedVariable(offset, _, _) => *offset,
            TypeError::UnknownField(offset, _, _, _) => *offset,
            TypeError::UnknownIndex {
                offset,
                span: _,
                index: _,
                tuple_type: _,
            } => *offset,
            TypeError::UnknownType(offset, _, _) => *offset,
        }
    }

    fn span(&self) -> &Span {
        match self {
            TypeError::Mismatch {
                offset: _,
                span,
                actual: _,
                expected: _,
            } => span,
            TypeError::MissingVariants(_, span, _) => span,
            TypeError::UndefinedVariable(_, span, _) => span,
            TypeError::UnknownField(_, span, _, _) => span,
            TypeError::UnknownIndex {
                offset: _,
                span,
                index: _,
                tuple_type: _,
            } => span,
            TypeError::UnknownType(_, span, _) => span,
        }
    }
}

impl From<TypeError> for Box<dyn Reporable> {
    fn from(val: TypeError) -> Self {
        Box::new(val)
    }
}

impl From<TypeError> for Vec<Box<dyn Reporable>> {
    fn from(val: TypeError) -> Self {
        vec![Box::new(val)]
    }
}

impl<T> From<TypeError> for Result<T, Vec<Box<dyn Reporable>>> {
    fn from(val: TypeError) -> Self {
        Err(vec![Box::new(val)])
    }
}
