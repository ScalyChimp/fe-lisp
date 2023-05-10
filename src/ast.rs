use std::error::Error;
use std::fmt::Display;

use chumsky::prelude::*;
use chumsky::Parser;
use env::Enviroment;
use expr::Expr;

pub fn eval(input: &str, env: &mut Enviroment) -> Result<Expr, LispError> {
    let ast = parser().parse(input).unwrap();
    // dbg!(&ast);
    ast.eval(env)
}

#[derive(Debug)]
pub enum LispError {
    /// TypeMismatch(ExpectedType, Actual Type)
    TypeMismatch(Expr, Expr),

    /// Symbol which couldn't be found in the environment.
    SymbolNotFound(String),
}

impl Error for LispError {}
impl Display for LispError {
    fn fmt(&self, mut f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LispError::TypeMismatch(expected, acquired) => write!(
                &mut f,
                "Type Mismatch: expected = {:?}, got = {:?}",
                expected, acquired
            ),
            LispError::SymbolNotFound(symbol) => {
                write!(
                    &mut f,
                    "Could not find symbol '{:?}' in environment",
                    symbol
                )
            }
        }
    }
}

pub mod env {
    use super::LispError;
    use crate::ast::expr::Expr;
    use std::collections::HashMap;

    pub fn default_env() -> Enviroment {
        let mut hash_map: HashMap<String, Expr> = HashMap::new();
        hash_map.insert(
            "+".to_string(),
            Expr::Fn(|args: &[Expr]| -> Result<Expr, LispError> {
                Ok(Expr::Number(
                    args.iter()
                        .map(|x| -> Result<&i64, LispError> {
                            if let Expr::Number(n) = x {
                                Ok(n)
                            } else {
                                Err(LispError::TypeMismatch(Expr::Number(0), x.clone()))
                            }
                        })
                        .sum::<Result<i64, _>>()?,
                ))
            }),
        );
        hash_map.insert(
            "-".to_string(),
            Expr::Fn(|args: &[Expr]| -> Result<Expr, LispError> {
                let first = &args[0];
                Ok(Expr::Number(
                    if let Expr::Number(n) = args[0] {
                        n
                    } else {
                        return Err(LispError::TypeMismatch(Expr::Number(0), first.clone()));
                    } - args[1..]
                        .iter()
                        .map(|x| {
                            if let Expr::Number(n) = x {
                                Ok(n)
                            } else {
                                Err(LispError::TypeMismatch(Expr::Number(0), first.clone()))
                            }
                        })
                        .sum::<Result<i64, _>>()?,
                ))
            }),
        );

        Enviroment { data: hash_map }
    }

    pub struct Enviroment {
        pub(super) data: HashMap<String, Expr>,
    }
}

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let num = text::int(10).from_str::<i64>().unwrapped();
    let negative_number = just('-').then(num).map(|x| Expr::Number(-x.1));

    let sym = filter::<char, _, Simple<char>>(|&c: &char| c != '(' && c != ')' && c != ' ')
        .repeated()
        .at_least(1)
        .at_most(64)
        .padded()
        .map(|x| x.iter().collect::<String>())
        .map(Expr::Symbol);

    let sexpr = recursive(|sexpr| {
        sexpr
            .padded()
            .repeated()
            .map(Expr::List)
            .delimited_by(just("("), just(")"))
            .or(negative_number)
            .or(num.map(Expr::Number))
            .or(sym)
    });

    sexpr
}

mod expr {

    use crate::ast::{Enviroment, LispError};
    use std::fmt;

    #[derive(Clone)]
    pub enum Expr {
        Fn(fn(&[Expr]) -> Result<Expr, LispError>),
        Symbol(String),
        Number(i64),
        List(Vec<Expr>),
    }

    impl Expr {
        pub fn eval(&self, env: &mut Enviroment) -> Result<Expr, LispError> {
            match self {
                Self::Number(n) => Ok(Expr::Number(*n)),
                Self::Symbol(s) => {
                    let data = env
                        .data
                        .get(s)
                        .ok_or_else(|| LispError::SymbolNotFound(s.to_string()))?;
                    Ok(data.clone())
                }
                Self::List(list) => match &list[..] {
                    [first, rest @ ..] => match first.eval(env)? {
                        Expr::Fn(function) => Ok(function(
                            &rest
                                .iter()
                                .map(|x| Self::eval(x, env))
                                .try_collect::<Vec<Expr>>()?[..],
                        )?),
                        _ => panic!("Not a function"),
                    },
                    _ => panic!("Cannot eval list"),
                },
                _ => panic!("cannot eval"),
            }
        }
    }

    impl fmt::Debug for Expr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Fn(_arg0) => f.debug_tuple("Fn").finish(),
                Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
                Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
                Self::List(arg0) => f.debug_tuple("List").field(arg0).finish(),
            }
        }
    }

    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let str = match self {
                Expr::Symbol(s) => s.clone(),
                Expr::Number(n) => n.to_string(),
                Expr::List(list) => {
                    let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                    format!("({})", xs.join(","))
                }
                Expr::Fn(_) => "Function".to_string(),
            };
            write!(f, "{}", str)
        }
    }
}
