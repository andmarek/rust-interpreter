use crate::{lexer::Lexer, token::Token, token::TokenType};
use std::io::{self, Write};

pub fn repl() -> io::Result<()> {
    // lexer: &Lexer
    let prompt = ">>";
    let mut input = String::new();

    loop {
        io::stdout().flush()?;
        input.clear();
        io::stdin().read_line(&mut input).unwrap();

        let trimmed_input = input.trim();

        if trimmed_input == "quit" {
            break;
        }

        println!("this {:?}", input);

        let l = Lexer::new(input.to_string());

        for token in l {
            println!("{:?}", token);
        }

        input.clear();
    }
    Ok(())
}
