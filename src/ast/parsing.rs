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
            "^" => "splice-unquote"
        );

        for (k, v) in lut.iter() {
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
