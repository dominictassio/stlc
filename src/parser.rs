mod span;

pub use span::Span;

use std::error::Error;

use indexmap::IndexMap;
use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser as PestParser,
};
use pest_derive::Parser;

use crate::syntax::{Abstraction, Declaration, Infix, Pattern, Prefix, Program, Term, Type};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Parser {
    source_name: String,
    type_parser: PrattParser<Rule>,
    expr_parser: PrattParser<Rule>,
}

impl Parser {
    pub fn new(source_name: String) -> Self {
        Parser {
            source_name,
            ..Default::default()
        }
    }

    pub fn parse_source(&self, source: &str) -> Result<Program, Box<dyn Error>> {
        let declarations = Parser::parse(Rule::program, source)?.next().unwrap();
        let declarations = declarations
            .into_inner()
            .map(|pair| self.parse_declaration(pair))
            .collect();
        Ok(Program(declarations))
    }

    fn parse_declaration(&self, pair: Pair<Rule>) -> Declaration {
        match pair.as_rule() {
            Rule::let_decl => {
                let mut inner_rules = pair.into_inner();
                let pattern = self.parse_pattern(inner_rules.next().unwrap());
                let term = self.parse_term(inner_rules.next().unwrap());
                Declaration::Term(pattern, term)
            }
            Rule::type_decl => {
                let mut inner_rules = pair.into_inner();
                let name = inner_rules.next().unwrap().as_str().to_string();
                let r#type = self.parse_type(inner_rules.next().unwrap());
                Declaration::Type(name, r#type)
            }
            _ => unreachable!("{pair}"),
        }
    }

    fn parse_type(&self, pair: Pair<Rule>) -> Type {
        self.type_parser
            .map_primary(|pair| {
                let span = self.span_from(&pair);
                match pair.as_rule() {
                    Rule::list_type => {
                        let mut inner_rules = pair.into_inner();
                        let value_type = self.parse_type(inner_rules.next().unwrap());
                        Type::List(span, Some(Box::new(value_type)))
                    }
                    Rule::record_type => {
                        let inner_rules = pair.into_inner();
                        let fields: IndexMap<String, Type> = inner_rules
                            .map(|pair| {
                                let mut inner_rules = pair.into_inner();
                                let label = inner_rules.next().unwrap().as_str().to_string();
                                let value_type = self.parse_type(inner_rules.next().unwrap());
                                (label, value_type)
                            })
                            .collect();
                        Type::Record(span, fields)
                    }
                    Rule::tuple_type => {
                        let inner_rules = pair.into_inner();
                        let mut types: Vec<Type> =
                            inner_rules.map(|pair| self.parse_type(pair)).collect();
                        match types.len() {
                            1 => types.remove(0),
                            _ => Type::Tuple(span, types),
                        }
                    }
                    Rule::variant_type => {
                        let inner_rules = pair.into_inner();
                        let variants: IndexMap<String, Type> = inner_rules
                            .map(|pair| {
                                let mut inner_rules = pair.into_inner();
                                let label = inner_rules.next().unwrap().as_str().to_string();
                                let variant_type = self.parse_type(inner_rules.next().unwrap());
                                (label, variant_type)
                            })
                            .collect();
                        Type::Variant(span, variants)
                    }
                    Rule::ident => Type::Variable(span, pair.as_str().to_string()),
                    _ => unreachable!(),
                }
            })
            .map_infix(|left, op, right| {
                let span = left.span().join(&self.span_from(&op)).unwrap();
                match op.as_rule() {
                    Rule::type_arrow => Type::Abstraction(span, Box::new(left), Box::new(right)),
                    _ => unreachable!(),
                }
            })
            .parse(pair.into_inner())
    }

    fn parse_term(&self, pair: Pair<Rule>) -> Term {
        let span = self.span_from(&pair);
        match pair.as_rule() {
            Rule::abs_term => {
                let mut inner_rules = pair.into_inner();
                let parameter = self.parse_pattern(inner_rules.next().unwrap());
                let parameter_type = self.parse_type(inner_rules.next().unwrap());
                let body = Box::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Abstraction(
                    Abstraction {
                        parameter,
                        parameter_type,
                        body,
                    },
                    span,
                )
            }
            Rule::fix_term => {
                let mut inner_rules = pair.into_inner();
                let value = Box::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Fix(value, span)
            }
            Rule::if_term => {
                let mut inner_rules = pair.into_inner();
                let condition = Box::new(self.parse_term(inner_rules.next().unwrap()));
                let consequent = Box::new(self.parse_term(inner_rules.next().unwrap()));
                let alternative = Box::new(self.parse_term(inner_rules.next().unwrap()));
                Term::If(condition, consequent, alternative, span)
            }
            Rule::let_term => {
                let mut inner_rules = pair.into_inner();
                let pattern = self.parse_pattern(inner_rules.next().unwrap());
                let value = Box::new(self.parse_term(inner_rules.next().unwrap()));
                let body = Box::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Let(pattern, value, body, span)
            }
            Rule::match_term => {
                let mut inner_rules = pair.into_inner();
                let value = Box::new(self.parse_term(inner_rules.next().unwrap()));
                let arms = inner_rules.map(|pair| self.parse_match_arm(pair)).collect();
                Term::Match(value, arms, span)
            }
            Rule::expr_term => self.parse_expr(pair),
            _ => unreachable!(),
        }
    }

    fn parse_match_arm(&self, pair: Pair<Rule>) -> (String, (Pattern, Term)) {
        let mut inner_rules = pair.into_inner();
        let label = inner_rules.next().unwrap().as_str().to_string();
        let pattern = self.parse_pattern(inner_rules.next().unwrap());
        let body = self.parse_term(inner_rules.next().unwrap());
        (label, (pattern, body))
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> Term {
        self.expr_parser
            .map_primary(|pair| {
                let inner_rules = pair.into_inner();
                inner_rules
                    .map(|pair| {
                        let mut inner_rules = pair.into_inner();
                        let primary = self.parse_primary(inner_rules.next().unwrap());
                        inner_rules.fold(primary, |term, pair| match pair.as_rule() {
                            Rule::ident => {
                                let span = self.span_from(&pair);
                                let label = pair.as_str().into();
                                Term::RecordProjection(Box::new(term), label, span)
                            }
                            Rule::nat => {
                                let span = self.span_from(&pair);
                                let index = pair.as_str().parse().unwrap();
                                Term::TupleProjection(Box::new(term), index, span)
                            }
                            _ => unreachable!(),
                        })
                    })
                    .reduce(|f, a| {
                        let span = f.span().join(a.span()).unwrap();
                        Term::Application(Box::new(f), Box::new(a), span)
                    })
                    .unwrap()
            })
            .map_prefix(|op, right| {
                let span = self.span_from(&op).join(right.span()).unwrap();
                match op.as_rule() {
                    Rule::neg_op => Term::Prefix(Prefix::Neg, Box::new(right), span),
                    Rule::not_op => Term::Prefix(Prefix::Not, Box::new(right), span),
                    _ => unreachable!(),
                }
            })
            .map_postfix(|left, op| {
                let span = left.span().join(&self.span_from(&op)).unwrap();
                match op.as_rule() {
                    Rule::r#as => {
                        let as_type = self.parse_type(op.into_inner().next().unwrap());
                        Term::Ascription(Box::new(left), as_type, span)
                    }
                    _ => unreachable!(),
                }
            })
            .map_infix(|left, op, right| {
                let span = left
                    .span()
                    .join(&self.span_from(&op))
                    .unwrap()
                    .join(right.span())
                    .unwrap();
                Term::Infix(
                    Box::new(left),
                    match op.as_rule() {
                        Rule::or_op => Infix::Or,
                        Rule::and_op => Infix::And,
                        Rule::eq_op => Infix::Eq,
                        Rule::ne_op => Infix::Ne,
                        Rule::ge_op => Infix::Ge,
                        Rule::gt_op => Infix::Gt,
                        Rule::le_op => Infix::Le,
                        Rule::lt_op => Infix::Lt,
                        Rule::add_op => Infix::Add,
                        Rule::sub_op => Infix::Sub,
                        Rule::mul_op => Infix::Mul,
                        Rule::div_op => Infix::Div,
                        Rule::rem_op => Infix::Rem,
                        Rule::pow_op => Infix::Pow,
                        _ => unreachable!(),
                    },
                    Box::new(right),
                    span,
                )
            })
            .parse(pair.into_inner())
    }

    fn parse_primary(&self, pair: Pair<Rule>) -> Term {
        let span = self.span_from(&pair);
        match pair.as_rule() {
            Rule::list_term => Term::List(
                pair.into_inner()
                    .map(|pair| self.parse_term(pair))
                    .collect(),
                span,
            ),
            Rule::record_term => Term::Record(
                pair.into_inner()
                    .map(|pair| self.parse_record_field(pair))
                    .collect(),
                span,
            ),
            Rule::tuple_term => {
                let inner_rules = pair.into_inner();
                let mut values: Vec<Term> = inner_rules.map(|pair| self.parse_term(pair)).collect();
                match values.len() {
                    1 => values.remove(0),
                    _ => Term::Tuple(values, span),
                }
            }
            Rule::variant_term => {
                let mut inner_rules = pair.into_inner();
                let label = inner_rules.next().unwrap().as_str().to_string();
                let value = Box::new(self.parse_term(inner_rules.next().unwrap()));
                Term::Variant(label, value, span)
            }
            Rule::ident => Term::Variable(pair.as_str().into(), span),
            Rule::nat => Term::Int(pair.as_str().parse().unwrap(), span),
            _ => unreachable!(),
        }
    }

    fn parse_record_field(&self, pair: Pair<Rule>) -> (String, Term) {
        let mut inner_rules = pair.into_inner();
        let label = inner_rules.next().unwrap().as_str().to_string();
        let value = self.parse_term(inner_rules.next().unwrap());
        (label, value)
    }

    fn parse_pattern(&self, pair: Pair<Rule>) -> Pattern {
        let span = self.span_from(&pair);
        match pair.as_rule() {
            Rule::record_pat => Pattern::Record(
                span,
                pair.into_inner()
                    .map(|pair| self.parse_field_pattern(pair))
                    .collect(),
            ),
            Rule::tuple_pat => Pattern::Tuple(
                span,
                pair.into_inner()
                    .map(|pair| self.parse_pattern(pair))
                    .collect(),
            ),
            Rule::wild_pat => Pattern::Wildcard(span),
            Rule::ident => {
                let name = pair.as_str().to_string();
                Pattern::Variable(span, name)
            }
            _ => unreachable!(),
        }
    }

    fn parse_field_pattern(&self, pair: Pair<Rule>) -> (String, Pattern) {
        let mut inner_rules = pair.into_inner();
        let label_pair = inner_rules.next().unwrap();
        let label = label_pair.as_str().to_string();
        let pattern = match inner_rules.next() {
            Some(pair) => self.parse_pattern(pair),
            None => Pattern::Variable(self.span_from(&label_pair), label.clone()),
        };
        (label, pattern)
    }

    fn span_from(&self, pair: &Pair<Rule>) -> Span {
        Span::from_pest(pair.as_span(), self.source_name.clone())
    }
}

impl Default for Parser {
    fn default() -> Self {
        let type_parser = PrattParser::new().op(Op::infix(Rule::type_arrow, Assoc::Right));
        let expr_parser = PrattParser::new()
            .op(Op::infix(Rule::or_op, Assoc::Left))
            .op(Op::infix(Rule::and_op, Assoc::Left))
            .op(Op::infix(Rule::eq_op, Assoc::Left) | Op::infix(Rule::ne_op, Assoc::Left))
            .op(Op::infix(Rule::ge_op, Assoc::Left)
                | Op::infix(Rule::gt_op, Assoc::Left)
                | Op::infix(Rule::le_op, Assoc::Left)
                | Op::infix(Rule::lt_op, Assoc::Left))
            .op(Op::infix(Rule::add_op, Assoc::Left) | Op::infix(Rule::sub_op, Assoc::Left))
            .op(Op::infix(Rule::mul_op, Assoc::Left)
                | Op::infix(Rule::div_op, Assoc::Left)
                | Op::infix(Rule::rem_op, Assoc::Left))
            .op(Op::infix(Rule::pow_op, Assoc::Right))
            .op(Op::postfix(Rule::r#as))
            .op(Op::prefix(Rule::neg_op) | Op::prefix(Rule::not_op));
        Self {
            source_name: "".into(),
            type_parser,
            expr_parser,
        }
    }
}
