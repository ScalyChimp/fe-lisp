use crate::ast::{Env, LispError};
use std::{collections::HashMap, fmt, rc::Rc, string::ToString};

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Fn,
    Symbol,
    Number,
    List,
    Bool,
}

#[derive(Clone)]
pub enum Expr {
    Symbol(String),
    Number(i64),
    Bool(bool),
    List(Vec<Expr>),
    Lambda(Lambda),
    Fn(fn(&[Expr], &mut Env) -> Result<Expr, LispError>),
}

#[derive(Clone, Debug)]
pub struct Lambda {
    /// Bindings in this context are the forms required by the lambda,
    /// which are then *bound* to the arugments that are passed to the lambda when it's called
    pub(super) bindings: Rc<Expr>,
    pub(super) body: Rc<Expr>,
}

impl Expr {
    pub fn eval(&self, env: &mut Env) -> Result<Self, LispError> {
        match self {
            Self::Number(n) => Ok(Self::Number(*n)),
            Self::Bool(n) => Ok(Self::Bool(*n)),
            Self::Symbol(s) => {
                let data =
                    env_get(s, env).ok_or_else(|| LispError::SymbolNotFound(s.to_string()))?;
                Ok(data)
            }
            Self::List(list) => match &list[..] {
                [first, rest @ ..] => match first.eval(env)? {
                    Self::Fn(func) => Ok(func(rest, env)?),
                    Self::Lambda(lambda) => {
                        let new_env = &mut create_lambda_scope(&lambda.bindings, rest, env)?;
                        lambda.body.eval(new_env)
                    }
                    not_a_fn => Err(LispError::TypeMismatch(Type::Fn, not_a_fn)),
                },
                _ => Err(LispError::MalformedList(list.clone())),
            },
            Self::Fn(x) => Err(LispError::TypeMismatch(Type::List, Self::Fn(*x))),
            Self::Lambda(x) => Err(LispError::TypeMismatch(Type::List, Self::Lambda(x.clone()))),
        }
    }
}

fn create_lambda_scope<'a>(
    bindings: &Rc<Expr>,
    args: &[Expr],
    outer_env: &'a mut Env,
) -> Result<Env<'a>, LispError> {
    let symbols = parse_symbol_list(bindings)?;
    if symbols.len() != args.len() {
        return Err(LispError::LambdaArity);
    }

    let values = eval_forms(args, outer_env)?;
    let mut data = HashMap::new();

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
        .map(|expr| match expr {
            Expr::Symbol(s) => Ok(s.clone()),
            not_a_symbol => Err(LispError::TypeMismatch(Type::Symbol, not_a_symbol.clone())),
        })
        .collect()
}

pub(super) fn eval_forms(args: &[Expr], env: &mut Env) -> Result<Vec<Expr>, LispError> {
    args.iter().map(|expr| Expr::eval(expr, env)).collect()
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
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Self::Symbol(s) => s.clone(),
            Self::Bool(b) => b.to_string(),
            Self::Number(n) => n.to_string(),
            Self::Fn(_) => "#<builtin>".to_string(),
            Self::Lambda(_) => "#<function>".to_string(),
            Self::List(list) => {
                let xs: Vec<String> = list.iter().map(ToString::to_string).collect();
                format!("'({})", xs.join(" "))
            }
        };
        write!(f, "{}", str)
    }
}
