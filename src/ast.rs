use chumsky::Parser;
use std::error::Error;
use std::fmt::Display;

pub mod env;
mod expr;
mod parsing;

use crate::Env;
use expr::{Expr, Type};

pub fn eval_expr(input: &str, env: &mut Env) -> Result<Expr, LispError> {
    let ast = parsing::parse_expr().parse(input).unwrap();
    ast.eval(env)
}

pub fn eval_script(input: &str, env: &mut Env) -> Result<Expr, LispError> {
    let ast = parsing::parse_script().parse(input).unwrap();
    for expr in &ast[..ast.len() - 1] {
        expr.eval(env)?;
    }
    ast[ast.len() - 1].eval(env)
}

#[derive(Debug)]
pub enum LispError {
    /// TypeMismatch (ExpectedType, ActualType)
    TypeMismatch(Type, Expr),

    /// Symbol which couldn't be found in the environment.
    SymbolNotFound(String),

    /// List which couldn't be evaulated.
    MalformedList(Vec<Expr>),

    /// Wrong number of arguments
    LambdaArity,
}

impl Error for LispError {}
impl Display for LispError {
    fn fmt(&self, mut f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(expected, acquired) => write!(
                &mut f,
                "Type Mismatch: expected = {:?}, got = {:?}",
                expected, acquired
            ),
            Self::SymbolNotFound(symbol) => {
                write!(
                    &mut f,
                    "Could not find symbol '{:?}' in environment",
                    symbol
                )
            }
            Self::MalformedList(list) => {
                write!(&mut f, "Could not eval list '{:?}' in environment", list)
            }
            Self::LambdaArity => {
                write!(&mut f, "Wrong number of forms expected for lambda form")
            }
        }
    }
}
