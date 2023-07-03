use crate::ast::Expr;
use chumsky::prelude::*;
use chumsky::Parser;

pub fn parse_expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    let positive_num = text::int(10).from_str::<i64>().unwrapped();
    let negative_num = just('-').then(positive_num).map(|x| -x.1);
    let float = negative_num
        .or(positive_num)
        .then(just('.').or_not().ignore_then(positive_num.or_not()))
        .map(|x| {
            let int_part = x.0 as f64;
            let dec_part = if let Some(n) = x.1 { n } else { 0 };
            let dec_part = dec_part as f64
                * 10.0_f64.powi(-(dec_part.checked_ilog10().unwrap_or(0) as i32 + 1_i32)) // this turns any number into a fractional part
                * int_part.signum();
            int_part + dec_part
        });

    let bool = choice((
        text::keyword("true").to(Expr::Bool(true)),
        text::keyword("false").to(Expr::Bool(false)),
    ));

    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Expr::String);

    let line_comment = just(";;").then(take_until(text::newline())).ignored();
    let block_comment = just("#|").then(take_until(just("|#"))).ignored();
    let comments = choice((line_comment, block_comment)).padded().repeated();

    let sym = filter(|&c: &char| c != '(' && c != ')' && c != ' ' && c != '"' && c != '#')
        .repeated()
        .at_least(1)
        .at_most(64)
        .padded()
        .collect::<String>()
        .map(|x| Expr::Symbol(x.trim().to_string()));

    let expr = recursive(|expr| {
        choice((
            expr.padded()
                .repeated()
                .map(Expr::List)
                .delimited_by(just("("), just(")")),
            float.map(Expr::Float),
            bool,
            string,
            sym,
        ))
        .padded_by(comments.or_not())
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
            "^" => "splice-unquote"
        );

        for (k, v) in &lut {
            while let Some(idx) = result.find(k) {
                let starts_with_bracket = result[idx + 1..].starts_with('(');

                result = result.replacen(k, "", 1);
                result.insert_str(idx, v);

                let offset = idx + v.len();
                if let Some(idx) = &result[offset..].find(' ') && !starts_with_bracket {
                        result.insert(offset + idx, ')');
                    } else {
                        match find_end_of_expr(&result[offset..]) {
                            Some(idx) => result.insert(offset+idx, ')'),
                            None => result.push(')'),
                        };
                    }
            }
        }

        result
    }

    fn find_end_of_expr(input: &str) -> Option<usize> {
        let input = input.to_string();
        let mut stack = vec![];
        for (index, char) in input.chars().enumerate() {
            if char == ')' {
                if stack.is_empty() {
                    return Some(index);
                }
                if stack.len() == 1 && stack.last() == Some(&'(') {
                    return Some(index);
                }
                if stack.last() == Some(&'(') {
                    stack.pop();
                } else {
                    stack.push(char);
                }
            } else if char == '(' {
                stack.push(char);
            }
        }
        None
    }

    #[test]
    fn quasiquote_reader_macro() {
        let input = r#"`(def ,name (fn ,args ^body))"#;
        let output =
            r#"(quasiquote (def (unquote name) (fn (unquote args) (splice-unquote body))))"#;
        let result = apply_reader_macros(input);
        assert_eq!(result, output);
    }
}
