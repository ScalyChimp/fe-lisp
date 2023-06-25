use crate::ast::Expr;
use chumsky::prelude::*;
use chumsky::Parser;

pub fn parse_expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    let positive_num = text::int(10).from_str::<i64>().unwrapped();
    let negative_num = just('-').then(positive_num).map(|x| Expr::Number(-x.1));
    let bool = choice((
        text::keyword("true").to(Expr::Bool(true)),
        text::keyword("false").to(Expr::Bool(false)),
    ));

    let sym = filter(|&c: &char| c != '(' && c != ')' && c != ' ')
        .repeated()
        .at_least(1)
        .at_most(64)
        .padded()
        .map(|x| x.iter().collect::<String>())
        .map(|x| Expr::Symbol(x.trim().to_string()));

    let expr = recursive(|expr| {
        expr.padded()
            .repeated()
            .map(Expr::List)
            .delimited_by(just("("), just(")"))
            .or(negative_num)
            .or(positive_num.map(Expr::Number))
            .or(bool)
            .or(sym)
    });

    expr
}
pub fn parse_script() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
    let expr = parse_expr();

    expr.padded().repeated()
}

pub mod reader_macros {
    macro_rules! reader_macro {
    ($($k:expr => $v:expr),+ $(,)? ) => {{
        let mut map: ::rustc_hash::FxHashMap<String, String>  = ::rustc_hash::FxHashMap::default();
        $(map.insert($k.to_string(), format!("({} ", $v.to_string()));)+
        map
    }};
}

    pub fn apply_reader_macros(input: &str) -> String {
        let mut result = input.to_string();
        let lut = reader_macro!(
            "'" => "quote",
            "`" => "quasiquote",
            "," => "unquote",
            ",;" => "splice-unquote"
        );

        for (k, v) in lut.iter() {
            if let Some(idx) = result.find(k) {
                result = result.replace(k, "");
                result.insert_str(idx, v);
                if let Some(new_idx) = result[idx + v.len()..].find(')') {
                    result.insert(idx + new_idx + v.len(), ')');
                } else if let Some(new_idx) = result[idx + v.len()..].find(' ') {
                    result.insert(idx + new_idx + v.len(), ')');
                }
            }
        }

        dbg!(result)
    }
}
