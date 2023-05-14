use crate::ast::{Env, LispError};
use std::{collections::HashMap, fmt, rc::Rc, string::ToString};

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Type {
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
                        let new_env = &mut create_lambda_scope(&lambda.arguments, rest, env)?;
                        lambda.body.eval(new_env)
                    }
                    x => Err(LispError::TypeMismatch(Type::Fn, x)),
                },
                _ => Err(LispError::MalformedList(list.clone())),
            },
            Self::Fn(x) => Err(LispError::TypeMismatch(Type::List, Self::Fn(*x))),
            Self::Lambda(x) => Err(LispError::TypeMismatch(Type::List, Self::Lambda(x.clone()))),
        }
    }
}

fn create_lambda_scope<'a>(
    params: &Rc<Expr>,
    arg_forms: &[Expr],
    outer_env: &'a mut Env,
) -> Result<Env<'a>, LispError> {
    let symbols = parse_symbol_list(params)?;
    if symbols.len() != arg_forms.len() {
        return Err(LispError::LambdaArity);
    }

    let values = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, Expr> = HashMap::new();

    for (k, v) in symbols.iter().zip(values.iter()) {
        data.insert(k.clone(), v.clone());
    }

    Ok(Env {
        data,
        outer: Some(outer_env),
    })
}

/// Parses a list of `Expr::Symbols` into a `Vec<String>`
/// to be used in getting values from the environment
fn parse_symbol_list(form: &Rc<Expr>) -> Result<Vec<String>, LispError> {
    let list = match form.as_ref() {
        Expr::List(s) => Ok(s.clone()),
        not_a_list => Err(LispError::TypeMismatch(Type::List, not_a_list.clone())),
    }?;

    list.iter()
        .map(|x| match x {
            Expr::Symbol(s) => Ok(s.clone()),
            x => Err(LispError::TypeMismatch(Type::Symbol, x.clone())),
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
                let xs: Vec<String> = list.iter().map(ToString::to_string).collect();
                format!("({})", xs.join(","))
            }
        };
        write!(f, "{}", str)
    }
}
