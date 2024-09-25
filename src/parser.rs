use crate::ast::{Identifier, LetStatement, Program, Statement, StatementType};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

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
        // while we haven't reached the end of the file
        let program = Program::new();

        program
    }

    pub fn parse_statement(&mut self) -> StatementType {
        match &self.cur_token {
            Some(token) => match token.token_type {
                TokenType::Let => StatementType::Let(self.parse_let_statement().unwrap()),
                _ => panic!("Unexpected token {:?}", token.token_type),
            },
            None => panic!("Unexpected end of file"),
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        let token = self.cur_token.clone().ok_or("No current token")?;

        let mut statement = LetStatement {
            token,
            name: None,
            value: None,
        };

        if !self.expect_peek(TokenType::Ident) {
            return Err("Expected identifier".to_string());
        }

        let ident_token = self.cur_token.clone().ok_or("No current token")?;
        statement.name = Some(Identifier::new(ident_token));

        // This is supposed to be assign
        if !self.expect_peek(TokenType::Equals) {
            return Err("Expected identifier".to_string());
        }
        Ok(statement)
    }

    pub fn cur_token_is(&self, token_type: TokenType) -> bool {
        if let Some(token) = &self.cur_token {
            token.token_type == token_type
        } else {
            false
        }
    }

    pub fn peek_token_is(&self, token_type: TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            token.token_type == token_type
        } else {
            false
        }
    }

    pub fn expect_peek(&self, token_type: TokenType) -> bool {
        match &self.peek_token {
            Some(token) => token.token_type == token_type,
            None => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let input = String::from("let x = 5;");
        let mut lexer = Lexer::new(input);
        let mut p = Parser::new(lexer);

        let program = p.parse_program();

        if program.statements.len() != 1 {
            panic!("Expected 1 statement, got {:?}", program.statements.len());
        }
    }

    // #[test]
    // fn test_parse_program() {
    //     let input = String::from(
    //         "let five = 5;
    //             let ten = 10;
    //             let add = fn(x, y) {
    //                 x + y;
    //             };
    //             let result = add(five, ten);
    //             ",
    //     );
    //     let mut parser = Parser::new(Lexer::new(input));
    //     let program = parser.parse_program().unwrap();
    //     if !program.token_literal().is_empty() {
    //         panic!("Failed to parse program");
    //     }
    //     if program.statements.len() != 3 {
    //         panic!("Expected 3 statements, got {}", program.statements.len());
    //     }
    // }
}
