use chumsky::Parser;
use std::error::Error;
use std::fmt::Display;

mod expr;
mod parsing;

use env::Env;
use expr::{Expr, Type};

pub fn eval(input: &str, env: &mut Env) -> Result<Expr, LispError> {
    let ast = parsing::parser().parse(input).unwrap();
    // dbg!(&ast);
    ast.eval(env)
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
            LispError::MalformedList(list) => {
                write!(&mut f, "Could not eval list '{:?}' in environment", list)
            }
            LispError::LambdaArity => {
                write!(&mut f, "Wrong number of forms expected for lambda form")
            }
        }
    }
}

pub mod env {
    use super::{
        expr::{eval_forms, Expr, Lambda, Type},
        LispError,
    };
    use std::{collections::HashMap, rc::Rc};

    #[macro_export]
    macro_rules! env {
        () => {{
            let map: HashMap<String, Expr> = ::std::collections::HashMap::new();
            map
        }};
        ($($k:expr => $v:expr),+ $(,)? ) => {{
            let mut map: HashMap<String, Expr>  = ::std::collections::HashMap::new();
            $(map.insert($k.to_string(), Expr::Fn($v));)+
            map
        }};
    }
    impl<'a> Default for Env<'a> {
        fn default() -> Env<'a> {
            let data = env!(
            "+" =>
            |args, env| {
                let args = &eval_forms(args, env)?[..];
                Ok(Expr::Number(
                    args.iter()
                        .map(|x| -> Result<&i64, LispError> {
                            if let Expr::Number(n) = x {
                                Ok(n)
                            } else {
                                Err(LispError::TypeMismatch(Type::Number, x.clone()))
                            }
                        })
                        .sum::<Result<i64, _>>()?,
                ))},
            "-" =>
            |args, env| {
                let args = &eval_forms(args, env)?[..];
                let first = &args[0];
                Ok(Expr::Number(
                    if let Expr::Number(n) = args[0] {
                        n
                    } else {
                        return Err(LispError::TypeMismatch(Type::Number, first.clone()));
                    } - args[1..]
                        .iter()
                        .map(|x| {
                            if let Expr::Number(n) = x {
                                Ok(n)
                            } else {
                                Err(LispError::TypeMismatch(Type::Number, first.clone()))
                            }
                        })
                        .sum::<Result<i64, _>>()?,
                ))},
            "*" =>
            |args, env| {
                let args = &eval_forms(args, env)?[..];
                Ok(Expr::Number(
                    args.iter()
                        .map(|x| -> Result<&i64, LispError> {
                            if let Expr::Number(n) = x {
                                Ok(n)
                            } else {
                                Err(LispError::TypeMismatch(Type::Number, x.clone()))
                            }
                        })
                        .product::<Result<i64, _>>()?,
                ))},
            "/"  =>
            |args, env| {
                let args = &eval_forms(args, env)?[..];
                let first = &args[0];
                Ok(Expr::Number(
                    if let Expr::Number(n) = args[0] {
                        n
                    } else {
                        return Err(LispError::TypeMismatch(Type::Number, first.clone()));
                    } / args[1..]
                        .iter()
                        .map(|x| {
                            if let Expr::Number(n) = x {
                                Ok(n)
                            } else {
                                Err(LispError::TypeMismatch(Type::Number, first.clone()))
                            }
                        })
                        .product::<Result<i64, _>>()?,
                ))},
            "fn" =>
            |args, _env| {
                let parameters = args.first().ok_or(LispError::LambdaArity)?;
                let body = args.get(1).ok_or(LispError::LambdaArity)?;
                if args.len() > 2 { return Err(LispError::LambdaArity) };
                Ok(Expr::Lambda(
                    Lambda {
                        body: Rc::new(body.clone()),
                        arguments: Rc::new(parameters.clone()),
                    }))},
            "def" =>
            |args, env| {
                let first = &args[0];
                let first_str = match first {
                    Expr::Symbol(s) => Ok(s.clone()),
                    x => Err(LispError::TypeMismatch(Type::Symbol, x.clone()))
                }?;
                let second_form = args.get(1).ok_or(
                    LispError::LambdaArity
                )?;
                if args.len() > 2 {
                    return Err(LispError::LambdaArity)
                }
                let second_eval =  second_form.eval(env)?;
                env.data.insert(first_str, second_eval);

                Ok(first.clone())}
            );

            Env { data, outer: None }
        }
    }

    pub struct Env<'a> {
        pub(super) data: HashMap<String, Expr>,
        pub(super) outer: Option<&'a Env<'a>>,
    }
}
