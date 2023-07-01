use super::{
    expr::{eval_forms, Expr, Lambda, Macro, Type},
    LispError,
};
use rustc_hash::FxHashMap as HashMap;
use std::{rc::Rc, time::Instant};

macro_rules! tonicity {
    ($op:tt) => {{
        |args, env| {
            let args = parse_nums(&args, env)?;
            fn op(a: i64, b: i64) -> bool { a $op b }
            let is_tonic = args.windows(2).all(|x| op(x[0], x[1]));
            Ok(Expr::Bool(is_tonic))
        }
    }};
}

fn parse_nums(list: &[Expr], env: &mut Env) -> Result<Vec<i64>, LispError> {
    list.iter()
        .map(|expr| match expr.eval(env) {
            Ok(Expr::Number(n)) => Ok(n),
            Ok(not_a_number) => Err(LispError::TypeMismatch(Type::Number, not_a_number)),
            Err(e) => Err(e),
        })
        .collect()
}

macro_rules! env {
    ($($k:expr => $v:expr),+ $(,)? ) => {{
        let mut map: ::rustc_hash::FxHashMap<String, Expr>  = ::rustc_hash::FxHashMap::default();
        $(map.insert($k.to_string(), Expr::Fn($v));)+
        map
    }};
}

impl<'a> Default for Env<'a> {
    fn default() -> Env<'a> {
        let data = env!(
        "+" =>
        |args, env| {
            let args = &parse_nums(args, env)?[..];
            Ok(Expr::Number(args.iter().sum()))
        },
        "-" =>
        |args, env| {
            let args = &parse_nums(args, env)?[..];
            let first = &args[0];
            Ok(Expr::Number(
                first
                 - args[1..]
                    .iter()
                    .sum::<i64>()))
        },
        "*" =>
        |args, env| {
            let args = &parse_nums(args, env)?[..];
            Ok(Expr::Number(args.iter().product()))
        },
        "/"  =>
        |args, env| {
            let args = &parse_nums(args, env)?[..];
            let first = &args[0];
            Ok(Expr::Number(
                first
                 / args[1..]
                    .iter()
                    .product::<i64>()))
        },
        "fn" =>
        |args, _env| {
            let parameters = args.first().ok_or(LispError::Arity)?;
            let body = args.get(1).ok_or(LispError::Arity)?;
            if args.len() > 2 { return Err(LispError::Arity) };
            Ok(Expr::Lambda(
                Lambda {
                    body: Rc::new(body.clone()),
                    bindings: Rc::new(parameters.clone())
                }
            ))
        },
        "macro" => // TODO: remove this code duplication
        |args, _env| {
            let parameters = args.first().ok_or(LispError::Arity)?;
            let body = args.get(1).ok_or(LispError::Arity)?;
            if args.len() > 2 { return Err(LispError::Arity) };
            Ok(Expr::Macro(
                Macro {
                    body: Rc::new(body.clone()),
                    bindings: Rc::new(parameters.clone())
                }
            ))
        },
        "m-expand1" =>
        |args, env| {
            let macroed = args[0].expand_once(env)?;
            Ok(macroed)
        },
        "quote" =>
        |args, _env| {
            Ok(args[0].clone())
        },
        "quasiquote" =>
        |args, env| {
            quasiquote(args, env)
        },
        "def" =>
        |args, env| {
            let first = &args[0];
            let first_str = match first {
                Expr::Symbol(s) => Ok(s.clone()),
                x => Err(LispError::TypeMismatch(Type::Symbol, x.clone()))
            }?;
            let second_form = args.get(1).ok_or(
                LispError::Arity
            )?;
            if args.len() > 2 {
                return Err(LispError::Arity)
            }
            let second_eval =  second_form.eval(env)?;
            env.data.insert(first_str, second_eval);

            Ok(first.clone())
        },
        "=" => tonicity!(==),
        "<" => tonicity!(<),
        ">" => tonicity!(>),
        "<=" => tonicity!(<=),
        ">=" => tonicity!(>=),
        "if" =>
        |args, env| {
            if args.len() > 3 { return Err(LispError::Arity) };
            let test = &args[0];
            match test.eval(env) {
                Ok(Expr::Bool(true)) => args[1].eval(env),
                Ok(Expr::Bool(false)) => args[2].eval(env),
                Err(e) => Err(e),
                Ok(not_bool) => Err(LispError::TypeMismatch(Type::Bool, not_bool))
            }
        },
        "do" =>
        |args, env| {
            let rest = &args[..args.len()];
            let mut env = Env::with_outer(env);
            let _ = eval_forms(rest, &mut env)?;
            args.last().expect("args list should not be empty").eval(&mut env) // TODO: Fix possible panic.
        },
        "let" =>
        |args, env| {
            if args.len() != 2 { return Err(LispError::Arity) };
            let body = &args[1];
            let bindings = match args.first().unwrap() {
                Expr::List(list) => list,
                not_a_list => Err(LispError::TypeMismatch(Type::List, not_a_list.clone()))?,
            };
            let mut env = Env::with_outer(env);
            bindings.chunks(2).map(|pair| {
                let symbol = &pair[0];
                let value = &pair[1];
                let symbol = match symbol {
                    Expr::Symbol(s) => Ok(s.clone()),
                    x => Err(LispError::TypeMismatch(Type::Symbol, x.clone()))
                }?;
                let evaluated = value.eval(&mut env)?;
                env.data.insert(symbol, evaluated);
                Ok(())
            }).try_collect()?;

            body.eval(&mut env)
        },
        "dbg" =>
        |args, env| {
            if args.len() != 1 { return Err(LispError::Arity) };
            let result = args[0].eval(env);
            dbg!(&result);
            result
        },
        "print" =>
        |args, env| {
            if args.len() != 1 { return Err(LispError::Arity) };
            let result = args[0].eval(env)?;
            println!("{}", result);
            Ok(result)
        },
        "time" =>
        |args, env| {
            if args.len() != 1 { return Err(LispError::Arity) };
            let start = Instant::now();
            let result = args[0].eval(env)?;
            let end = Instant::now();
            let difference = end - start;
            println!("Eval time for expr: {} = {:?}", args[0], difference);
            Ok(result)
        },
        );

        Env { data, outer: None }
    }
}

#[derive(Debug)]
pub struct Env<'a> {
    pub(super) data: HashMap<String, Expr>,
    pub(super) outer: Option<&'a Env<'a>>,
}

impl Env<'_> {
    fn with_outer<'a>(env: &'a mut Env<'_>) -> Env<'a> {
        Env {
            outer: Some(env),
            data: HashMap::default(),
        }
    }

    pub fn get(&self, k: &str) -> Option<Expr> {
        match self.data.get(k) {
            Some(exp) => Some(exp.clone()),
            None => match &self.outer {
                Some(outer_env) => outer_env.get(k),
                None => None,
            },
        }
    }
}

fn quasiquote(args: &[Expr], env: &mut Env) -> Result<Expr, LispError> {
    use Expr::*;

    let args = args.to_vec();
    let mut results = vec![];
    for element in args.into_iter().rev() {
        match element {
            List(l) => match &l[..] {
                [Symbol(s), Symbol(k), rest @ ..] => {
                    if !rest.is_empty() {
                        Err(LispError::Arity)?
                    } else {
                        match s.as_ref() {
                            "unquote" => match env.get(k) {
                                Some(data) => results.push(data.clone()),
                                None => results.push(List(l.to_vec())),
                            },
                            "splice-unquote" => {
                                if let List(l) = &env
                                    .data
                                    .get(k)
                                    .ok_or(LispError::SymbolNotFound(k.to_string()))?
                                {
                                    results.append(&mut l.iter().cloned().rev().collect());
                                }
                            }
                            _ => results.push(List(l.to_vec())),
                        }
                    }
                }
                _ => results.push(quasiquote(&l[..], env)?),
            },
            not_a_list => results.push(not_a_list),
        };
    }
    Ok(List(results.into_iter().rev().collect()))
}
