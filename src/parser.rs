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
        self.peek_token = self.lexer.next();
        self.cur_token = self.peek_token.take();
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        println!("parse program");
        self.next_token();
        //
        // while we haven't reached the end of the file
        let mut program = Program::new();

        println!("cur token is {:?}", self.cur_token);

        while let Some(_) = self.cur_token {
            println!("HELP");
            let statement = self.parse_statement()?;
            let boxed_statement: Box<dyn Statement> = match statement {
                StatementType::Let(let_stmt) => Box::new(let_stmt),
            };
            program.statements.push(boxed_statement);
        }
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> Result<StatementType, String> {
        println!("parse statement");
        match &self.cur_token {
            Some(token) => match token.token_type {
                TokenType::Let => self.parse_let_statement().map(StatementType::Let),
                _ => panic!("Unexpected token {:?}", token.token_type),
            },
            None => panic!("Unexpected end of file"),
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        println!("parse let statement");
        let token = self.cur_token.clone().ok_or("No current token")?;

        let mut statement = LetStatement {
            token,
            name: None,
            value: None,
        };

        /* If we don't have an identifier, then it's not a valid let statement */
        if !self.expect_peek(TokenType::Ident) {
            return Err("Expected identifier".to_string());
        }

        let ident_token = self.cur_token.clone().ok_or("No current token")?;
        statement.name = Some(Identifier::new(ident_token));

        /* We need to rename Equals to Assign to make the code more clear */
        if !self.expect_peek(TokenType::Equals) {
            return Err("Expected identifier".to_string());
        }

        /* Skipping over the rest of the let statement until semi colon for now */
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
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
        let lexer = Lexer::new(input);
        let mut p = Parser::new(lexer);

        let program = p.parse_program();
        if program.is_ok() {
            let unwrapped_program = program.unwrap();
            if unwrapped_program.statements.len() != 1 {
                panic!(
                    "Expected 1 statement, got {:?}",
                    unwrapped_program.statements.len()
                );
            }
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
