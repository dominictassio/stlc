use ariadne::{Color, Fmt, Label, Report, ReportKind, Span as AriadneSpan};

use crate::{
    parser::Span,
    syntax::{Pattern, Type},
};

use super::Reporable;

#[derive(Clone, Debug)]
pub enum PatternError {
    Incompatible {
        span: Span,
        actual: Pattern,
        expected: Type,
    },
    MissingElements {
        span: Span,
        actual: usize,
        expected: usize,
    },
    MissingFields(Span, Vec<String>),
    UnknownFields(Span, Vec<String>, Type),
}

impl Reporable for PatternError {
    fn build_report(&self) -> Report<Span> {
        let report = Report::<Span>::build(ReportKind::Error, self.span().source(), self.offset());
        match self {
            PatternError::Incompatible {
                span,
                actual: _,
                expected,
            } => report.with_message("mismatched types").with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "expected {}",
                        expected.to_string().fg(Color::Green),
                    ))
                    .with_color(Color::Red),
            ),
            PatternError::MissingElements {
                span,
                actual,
                expected,
            } => report.with_message("mismatched types").with_label(
                Label::new(span.clone())
                    .with_message(format!(
                        "expected a tuple with {} elements, found one with {} elements",
                        expected.to_string().fg(Color::Green),
                        actual.to_string().fg(Color::Red),
                    ))
                    .with_color(Color::Red),
            ),
            PatternError::MissingFields(span, fields) if fields.len() == 1 => report
                .with_message(format!(
                    "pattern does not mention field `{}`",
                    fields.first().unwrap()
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "missing field `{}`",
                            fields.first().unwrap().to_string().fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            PatternError::MissingFields(span, fields) => {
                let fields: Vec<String> =
                    fields.iter().map(|field| format!("`{}`", field)).collect();
                report
                    .with_message(format!(
                        "pattern does not mention fields {}",
                        fields.join(", ")
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "missing fields {}",
                                fields.join(", ").fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    )
            }
            PatternError::UnknownFields(span, fields, r#type) => {
                let fields: Vec<String> =
                    fields.iter().map(|field| format!("`{}`", field)).collect();
                report
                    .with_message(format!(
                        "record `{type}` does not have fields {}",
                        fields.join(", ")
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "record `{type}` does not have fields {}",
                                fields.join(", ").fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    )
            }
        }
        .finish()
    }

    fn offset(&self) -> usize {
        match self {
            PatternError::Incompatible { span, .. } => span.start(),
            PatternError::MissingElements { span, .. } => span.start(),
            PatternError::MissingFields(span, _) => span.start(),
            PatternError::UnknownFields(span, _, _) => span.start(),
        }
    }

    fn span(&self) -> &Span {
        match self {
            PatternError::Incompatible { span, .. } => span,
            PatternError::MissingElements { span, .. } => span,
            PatternError::MissingFields(span, _) => span,
            PatternError::UnknownFields(span, _, _) => span,
        }
    }
}

impl From<PatternError> for Box<dyn Reporable> {
    fn from(val: PatternError) -> Self {
        Box::new(val)
    }
}

impl From<PatternError> for Vec<Box<dyn Reporable>> {
    fn from(val: PatternError) -> Self {
        vec![Box::new(val)]
    }
}

impl<T> From<PatternError> for Result<T, Vec<Box<dyn Reporable>>> {
    fn from(val: PatternError) -> Self {
        Err(vec![Box::new(val)])
    }
}
