use super::{
    expr::{eval_forms, Expr, Lambda, Type},
    LispError,
};
use std::{collections::HashMap, rc::Rc};

macro_rules! tonicity {
    ($op:tt) => {{
        |args, _env| {
            let args = parse_nums(args)?;
            fn op(a: i64, b: i64) -> bool { a $op b }
            let is_tonic = args.windows(2).all(|x| op(x[0], x[1]));
            Ok(Expr::Bool(is_tonic))
        }
    }};
}

fn parse_nums(list: &[Expr]) -> Result<Vec<i64>, LispError> {
    list.iter()
        .map(|expr| match expr {
            Expr::Number(n) => Ok(n.clone()),
            not_a_number => Err(LispError::TypeMismatch(Type::Number, not_a_number.clone())),
        })
        .collect()
}

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
                    bindings: Rc::new(parameters.clone()),
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

            Ok(first.clone())},
        "=" => tonicity!(==),
        "<" => tonicity!(<),
        ">" => tonicity!(>),
        "<=" => tonicity!(<=),
        ">=" => tonicity!(>=),
        );

        Env { data, outer: None }
    }
}

pub struct Env<'a> {
    pub(super) data: HashMap<String, Expr>,
    pub(super) outer: Option<&'a Env<'a>>,
}
