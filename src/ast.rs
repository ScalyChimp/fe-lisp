use chumsky::Parser;
use std::error::Error;
use std::fmt::Display;

use env::Env;
use expr::{Expr, ExprType};

mod parsing;
pub fn eval(input: &str, env: &mut Env) -> Result<Expr, LispError> {
    let ast = parsing::parser().parse(input).unwrap();
    // dbg!(&ast);
    ast.eval(env)
}

#[derive(Debug)]
pub enum LispError {
    /// TypeMismatch (ExpectedType, ActualType)
    TypeMismatch(ExprType, Expr),

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
        expr::{eval_forms, Expr, ExprType, Lambda},
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

    pub fn default_env<'a>() -> Env<'a> {
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
                            Err(LispError::TypeMismatch(ExprType::Number, x.clone()))
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
                    return Err(LispError::TypeMismatch(ExprType::Number, first.clone()));
                } - args[1..]
                    .iter()
                    .map(|x| {
                        if let Expr::Number(n) = x {
                            Ok(n)
                        } else {
                            Err(LispError::TypeMismatch(ExprType::Number, first.clone()))
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
                            Err(LispError::TypeMismatch(ExprType::Number, x.clone()))
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
                    return Err(LispError::TypeMismatch(ExprType::Number, first.clone()));
                } / args[1..]
                    .iter()
                    .map(|x| {
                        if let Expr::Number(n) = x {
                            Ok(n)
                        } else {
                            Err(LispError::TypeMismatch(ExprType::Number, first.clone()))
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
                x => Err(LispError::TypeMismatch(ExprType::Symbol, x.clone()))
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

    pub struct Env<'a> {
        pub(super) data: HashMap<String, Expr>,
        pub(super) outer: Option<&'a Env<'a>>,
    }
}

mod expr {

    use crate::ast::{Env, LispError};
    use std::{collections::HashMap, fmt, rc::Rc};

    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)]
    pub enum ExprType {
        Fn,
        Symbol,
        Number,
        List,
    }

    #[derive(Clone)]
    pub enum Expr {
        Fn(fn(&[Expr], &mut Env) -> Result<Expr, LispError>),
        Symbol(String),
        Number(i64),
        List(Vec<Expr>),
        Lambda(Lambda),
    }

    #[derive(Clone, Debug)]
    #[allow(dead_code)]
    pub struct Lambda {
        pub(super) arguments: Rc<Expr>,
        pub(super) body: Rc<Expr>,
    }

    impl Expr {
        pub fn eval(&self, env: &mut Env) -> Result<Expr, LispError> {
            match self {
                Self::Number(n) => Ok(Expr::Number(*n)),
                Self::Symbol(s) => {
                    let data =
                        env_get(s, env).ok_or_else(|| LispError::SymbolNotFound(s.to_string()))?;
                    Ok(data)
                }
                Self::List(list) => match &list[..] {
                    [first, rest @ ..] => match first.eval(env)? {
                        Expr::Fn(func) => Ok(func(rest, env)?),
                        Expr::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.arguments, rest, env)?;
                            lambda.body.eval(new_env)
                        }
                        x => Err(LispError::TypeMismatch(ExprType::Fn, x)),
                    },
                    _ => Err(LispError::MalformedList(list.clone())),
                },
                Self::Fn(x) => Err(LispError::TypeMismatch(ExprType::List, Self::Fn(*x))),
                Self::Lambda(x) => Err(LispError::TypeMismatch(
                    ExprType::List,
                    Self::Lambda(x.clone()),
                )),
            }
        }
    }

    fn env_for_lambda<'a>(
        params: Rc<Expr>,
        arg_forms: &[Expr],
        outer_env: &'a mut Env,
    ) -> Result<Env<'a>, LispError> {
        let ks = parse_list_of_symbol_strings(params)?;
        if ks.len() != arg_forms.len() {
            return Err(LispError::LambdaArity);
        }
        let vs = eval_forms(arg_forms, outer_env)?;
        let mut data: HashMap<String, Expr> = HashMap::new();
        for (k, v) in ks.iter().zip(vs.iter()) {
            data.insert(k.clone(), v.clone());
        }
        Ok(Env {
            data,
            outer: Some(outer_env),
        })
    }

    fn parse_list_of_symbol_strings(form: Rc<Expr>) -> Result<Vec<String>, LispError> {
        let list = match form.as_ref() {
            Expr::List(s) => Ok(s.clone()),
            x => Err(LispError::TypeMismatch(ExprType::List, x.clone())),
        }?;
        list.iter()
            .map(|x| match x {
                Expr::Symbol(s) => Ok(s.clone()),
                x => Err(LispError::TypeMismatch(ExprType::Symbol, x.clone())),
            })
            .collect()
    }

    pub(super) fn eval_forms(args: &[Expr], env: &mut Env) -> Result<Vec<Expr>, LispError> {
        args.iter().map(|x| Expr::eval(x, env)).collect()
    }

    fn env_get(k: &str, env: &Env) -> Option<Expr> {
        match env.data.get(k) {
            Some(exp) => Some(exp.clone()),
            None => match &env.outer {
                Some(outer_env) => env_get(k, outer_env),
                None => None,
            },
        }
    }

    impl fmt::Debug for Expr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Fn(_) => f.debug_tuple("Fn").finish(),
                Self::Lambda(arg0) => f.debug_tuple("Lambda").field(arg0).finish(),
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
                Expr::Fn(_) => "Function".to_string(),
                Expr::Lambda(_) => "Lambda {}".to_string(),
                Expr::List(list) => {
                    let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                    format!("({})", xs.join(","))
                }
            };
            write!(f, "{}", str)
        }
    }
}
