use std::error::Error;
use std::{env, fs, process};

use ariadne::Source;
use eval::{Context, Environment, Prelude, Value};
use parser::Parser;
use syntax::{Declaration, Program, Term, Type};

use crate::parser::Span;

mod error;
mod eval;
mod parser;
mod syntax;

fn main() -> Result<(), Box<dyn Error>> {
    let Some(path) = env::args().nth(1) else {
        return Err("Usage: stlc <file>".into());
    };

    let source = fs::read_to_string(&path)?;
    let parser = Parser::new(path.clone());
    let Program(declarations) = parser.parse_source(&source)?;

    let mut context = Context::default();
    let mut environment = Environment::default();
    Prelude::default().add_prelude_to(&mut context, &mut environment);

    for declaration in &declarations {
        match declaration {
            Declaration::Term(pattern, term) => {
                let raw_type = context.type_of(term);
                let Ok(raw_type) = raw_type else {
                    let reports = raw_type.unwrap_err();
                    for report in reports {
                        report
                            .build_report()
                            .eprint((path.clone(), Source::from(&source)))?;
                    }
                    process::exit(1);
                };
                let r#type = context.resolve(raw_type);
                let Ok(r#type) = r#type else {
                    let reports = r#type.unwrap_err();
                    for report in reports {
                        report
                            .build_report()
                            .eprint((path.clone(), Source::from(&source)))?;
                    }
                    process::exit(1);
                };
                context.bind_pattern(pattern, term, r#type).unwrap();
                let value = environment.eval(term)?;
                environment.bind_pattern(pattern, value)?;
            }
            Declaration::Type(name, r#type) => {
                context.insert_type(name.clone(), r#type.clone());
            }
        }
    }

    let main_type = context.get(&"main".to_string()).unwrap();
    let main = environment.get(&"main".to_string())?;

    match (main, main_type) {
        (Value::Abstraction(abstraction, environment), Type::Abstraction(_, parameter_type, _))
            if (**parameter_type).is_tuple() =>
        {
            let span = Span::default();
            let value = environment.clone().eval(&Term::Application(
                Box::new(Term::Abstraction(abstraction.clone(), span.clone())),
                Box::new(Term::Tuple(vec![], span.clone())),
                span.clone(),
            ))?;
            println!("{value}");
        }
        (value, r#type) => {
            println!("{value} : {type}");
        }
    }

    Ok(())
}
