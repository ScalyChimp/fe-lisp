use crate::ast::Expr;
use chumsky::prelude::*;
use chumsky::Parser;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let num = text::int(10).from_str::<i64>().unwrapped();
    let negative_number = just('-').then(num).map(|x| Expr::Number(-x.1));

    let sym = filter::<char, _, Simple<char>>(|&c: &char| c != '(' && c != ')' && c != ' ')
        .repeated()
        .at_least(1)
        .at_most(64)
        .padded()
        .map(|x| x.iter().collect::<String>())
        .map(Expr::Symbol);

    let sexpr = recursive(|sexpr| {
        sexpr
            .padded()
            .repeated()
            .map(Expr::List)
            .delimited_by(just("("), just(")"))
            .or(negative_number)
            .or(num.map(Expr::Number))
            .or(sym)
    });

    sexpr
}
