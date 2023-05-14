#![feature(iterator_try_collect)]
use ::rustyline::error::ReadlineError;
use ast::env::Env;
pub use chumsky::{prelude::*, Parser};
pub use std::{
    error::Error,
    io::{self, stdout, Write as IoWrite},
};

mod ast;
mod rustyline;

fn main() -> Result<(), Box<dyn Error>> {
    println!("fe-lisp repl v0.0.1");
    repl()
}

fn repl() -> Result<(), Box<dyn Error>> {
    let mut rl = rustyline::config()?;
    let mut env = Env::default();

    let mut input = rl.readline("λ ");
    loop {
        match input {
            Ok(ref line) => {
                if line.is_empty() {
                    input = Ok(rl.readline("λ ")?);
                    continue;
                }

                rl.add_history_entry(line.as_str())?;

                let result = ast::eval(&input?, &mut env)?;
                input = rl.readline(&format!("{}\nλ ", result));
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => {
                println!("Interrupted, saving history...");
                rl.save_history("fe-lisp.history")?;
                println!("Done");
                break Ok(());
            }
            Err(err) => {
                println!("Error, saving history...");
                rl.save_history("fe-lisp.history")?;
                println!("Done");
                break Err(Box::new(err));
            }
        }
    }
}
