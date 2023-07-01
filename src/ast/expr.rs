use crate::ast::{Env, LispError};
use rustc_hash::FxHashMap as HashMap;
use std::{fmt, rc::Rc, string::ToString};

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
    Macro(Macro),
}

#[derive(Clone, Debug)]
pub struct Lambda {
    /// Bindings in this context are the forms required by the lambda,
    /// which are then *bound* to the arugments that are passed to the lambda when it's called
    pub(super) bindings: Rc<Expr>,
    pub(super) body: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub struct Macro {
    /// The difference between this and a lambda is that the arguments are passed unevaluated.
    /// Macros are also expanded before evaluating anything else.
    pub(super) bindings: Rc<Expr>,
    pub(super) body: Rc<Expr>,
}

impl Expr {
    /// Expands one level of macros.
    pub(super) fn expand_once(&self, env: &mut Env) -> Result<Expr, LispError> {
        use Expr::*;

        match self {
            List(list) => match &list[..] {
                [sym @ Symbol(_), args @ ..] => match sym.eval(env) {
                    Ok(Macro(m)) => {
                        let mut new_env = create_scope(&m.bindings, args, env)?;
                        m.body.eval(&mut new_env)
                    }
                    _ => Ok(self.clone()),
                },
                not_a_macro => Ok(List(not_a_macro.to_vec())),
            },
            not_a_list => Ok(not_a_list.clone()),
        }
    }

    /// Expands all macros in an ast.
    pub fn expand_all(&self, env: &mut Env) -> Result<Expr, LispError> {
        use Expr::*;

        let result = match self {
            List(list) => {
                let mut list = list.clone();
                for expr in list[1..].iter_mut() {
                    *expr = expr.expand_all(env)?
                }
                List(list).expand_once(env)
            }
            _ => Ok(self.clone()),
        };

        // TODO: This is to fix accidentally having a list in a list
        // which should maybe be fixed in a dif way but this work so.
        if let Ok(List(ref list)) = result && list.len() == 1 {
            match &list[0] {
                List(l) => Ok(List(l.to_vec())),
                _ => result,
            }
        } else {
            result
        }
    }

    pub fn eval(&self, env: &mut Env) -> Result<Self, LispError> {
        use Expr::*;
        use LispError::*;

        match self {
            Number(n) => Ok(Number(*n)),
            Bool(n) => Ok(Bool(*n)),
            Symbol(s) => {
                let data = env.get(s).ok_or_else(|| SymbolNotFound(s.to_string()))?;
                Ok(data)
            }
            List(list) => match &list[..] {
                [first, rest @ ..] => match first.eval(env)? {
                    Fn(func) => Ok(func(rest, env)?),
                    Lambda(lambda) => {
                        let args = eval_forms(rest, env)?;
                        let new_env = &mut create_scope(&lambda.bindings, &args, env)?;
                        lambda.body.eval(new_env)
                    }
                    not_a_fn => Err(TypeMismatch(Type::Fn, not_a_fn)),
                },
                _ => Err(MalformedList(list.clone())),
            },
            Fn(x) => Err(TypeMismatch(Type::List, Fn(*x))),
            Lambda(x) => Err(TypeMismatch(Type::List, Lambda(x.clone()))),
            Macro(_) => unreachable!("all macros should be expanded before evaluation"),
        }
    }
}

fn create_scope<'a>(
    bindings: &Rc<Expr>,
    args: &[Expr],
    outer_env: &'a mut Env,
) -> Result<Env<'a>, LispError> {
    let symbols = parse_symbol_list(bindings)?;
    if symbols.len() != args.len() {
        return Err(LispError::Arity);
    }

    let mut data = HashMap::default();

    for (k, v) in symbols.into_iter().zip(args.iter()) {
        data.insert(k, v.clone());
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

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fn(_) => f.debug_tuple("Fn").finish(),
            Self::Lambda(arg0) => f.debug_tuple("Lambda").field(arg0).finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::List(arg0) => f.debug_tuple("List").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Macro(arg0) => f.debug_tuple("Macro").field(arg0).finish(),
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
            Self::Macro(_) => "#<macro>".to_string(),
            Self::Lambda(_) => "#<function>".to_string(),
            Self::List(list) => {
                let xs: Vec<String> = list.iter().map(ToString::to_string).collect();
                format!("'({})", xs.join(" "))
            }
        };
        write!(f, "{}", str)
    }
}
