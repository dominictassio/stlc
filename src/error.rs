#![allow(unused_imports)]

mod eval_error;
mod pattern_error;
mod type_error;

use std::fmt::Debug;

use ariadne::Report;

pub use eval_error::EvalError;
pub use pattern_error::PatternError;
pub use type_error::TypeError;

use crate::parser::Span;

pub trait Reporable: Debug {
    fn build_report(&self) -> Report<Span>;
    fn offset(&self) -> usize;
    fn span(&self) -> &Span;
}
