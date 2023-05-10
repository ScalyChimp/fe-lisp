use rustyline::{
    error::ReadlineError, highlight::MatchingBracketHighlighter, history::FileHistory,
    validate::MatchingBracketValidator, Editor,
};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter, Validator};

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct Config {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighting: MatchingBracketHighlighter,
}

pub fn config() -> Result<Editor<Config, FileHistory>, ReadlineError> {
    let config = Config {
        brackets: MatchingBracketValidator::new(),
        highlighting: MatchingBracketHighlighter::new(),
    };
    let mut rl = Editor::new()?;
    rl.set_helper(Some(config));
    match rl.load_history("fe-lisp.history") {
        Ok(()) => println!("history loaded from fe-lisp.history"),
        Err(err) => eprint!("error loading history from fe-lisp.history: {}", err),
    }

    Ok(rl)
}
