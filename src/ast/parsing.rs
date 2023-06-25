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
