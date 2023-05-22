use super::{
    expr::{Expr, Lambda, Type},
    LispError,
};
use std::{collections::HashMap, rc::Rc, time::Instant};

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
        .map(|e| e.eval(env))
        .map(|expr| match expr {
            Ok(Expr::Number(n)) => Ok(n),
            Ok(not_a_number) => Err(LispError::TypeMismatch(Type::Number, not_a_number)),
            Err(e) => Err(e),
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
            let parameters = args.first().ok_or(LispError::LambdaArity)?;
            let body = args.get(1).ok_or(LispError::LambdaArity)?;
            if args.len() > 2 { return Err(LispError::LambdaArity) };
            Ok(Expr::Lambda(
                Lambda {
                    body: Rc::new(body.clone()),
                    bindings: Rc::new(parameters.clone())
                }
            ))
        },
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

            Ok(first.clone())
        },
        "=" => tonicity!(==),
        "<" => tonicity!(<),
        ">" => tonicity!(>),
        "<=" => tonicity!(<=),
        ">=" => tonicity!(>=),
        "if" =>
        |args, env| {
            if args.len() > 3 { return Err(LispError::LambdaArity) };
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
            let _: Vec<_> = args[..args.len()].iter().map(|e| e.eval(env)).try_collect()?;
            args.last().expect("args list should not be empty").eval(env)
        },
        "let" =>
        |args, env| {
            if args.len() != 2 { return Err(LispError::LambdaArity) };
            let body = &args[1];
            let bindings = match args.first().unwrap() {
                Expr::List(list) => list,
                not_a_list => Err(LispError::TypeMismatch(Type::List, not_a_list.clone()))?,
            };
            let mut env = Env { data: HashMap::new(), outer: Some(env) };
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
            if args.len() != 1 { return Err(LispError::LambdaArity) };
            let result = args[0].eval(env);
            dbg!(&result);
            result
        },
        "print" =>
        |args, env| {
            if args.len() != 1 { return Err(LispError::LambdaArity) };
            let result = args[0].eval(env)?;
            println!("{}", result);
            Ok(result)
        }
        "time" =>
        |args, env| {
            if args.len() != 1 { return Err(LispError::LambdaArity) };
            let start = Instant::now();
            let result = args[0].eval(env)?;
            let end = Instant::now();
            let eval_time = end - start;
            println!("Eval time for expr: {} = {:?}", args[0], eval_time);
            Ok(result)
        },
        );

        Env { data, outer: None }
    }
}

pub struct Env<'a> {
    pub(super) data: HashMap<String, Expr>,
    pub(super) outer: Option<&'a Env<'a>>,
}
