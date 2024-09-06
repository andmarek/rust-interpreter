use crate::ast::Program;
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser {
            lexer,
            cur_token: None,
            peek_token: None,
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(&mut self) -> Program {
        return None;
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_program() {
        let input = String::from(
            "let five = 5;
                let ten = 10;
                let add = fn(x, y) {
                    x + y;
                };
                let result = add(five, ten);
                ",
        );
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();
        if program.is_none() {
            panic!("Failed to parse program");
        }
    }
}
