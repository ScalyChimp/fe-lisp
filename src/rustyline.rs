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
    match rl.load_history("wilf.history") {
        Ok(()) => println!("history loaded from wilf.history"),
        Err(err) => eprint!("error loading history from wilf.history: {}", err),
    }

    Ok(rl)
}
