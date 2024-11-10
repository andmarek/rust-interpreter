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
        println!("{:?}", lexer.get_input());
        let mut parser = Parser {
            lexer,
            cur_token: None,
            peek_token: None,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());

        println!("cur_token is {:?}", self.cur_token);
        println!(
            "peek_token is {:?}",
            self.peek_token.clone().unwrap().literal
        );
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program::new();

        println!("Beginning to parse the program");

        while let Some(token) = &self.cur_token {
            println!("Parsing the program, the current token is {:?}", token);

            let statement = self.parse_statement();

            let boxed_statement: Box<dyn Statement> = match statement {
                Some(Ok(StatementType::Let(let_stmt))) => Box::new(let_stmt),
                Some(Err(err)) => return Err(err),
                None => break,
            };

            println!("Statement is {:?}", boxed_statement);

            program.statements.push(boxed_statement);

            self.next_token();
        }
        print!("{:?}", program.statements);
        println!("We're done parsing the program.");
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> Option<Result<StatementType, String>> {
        println!("parse statement");
        match &self.cur_token {
            Some(token) => match token.token_type {
                TokenType::Eof => None,
                TokenType::Let => Some(self.parse_let_statement().map(StatementType::Let)),
                _ => Some(Err(format!("Unexpected token {:?}", token.token_type))),
            },
            None => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
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

    pub fn expect_peek(&mut self, token_type: TokenType) -> bool {
        // if the token is what we expect, then we move the parser to the next
        // token and return true
        // else return false
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_single_let_statement() {
        let input = String::from("let x = 5;");
        println!("Testing with input: {}", input); // This will print

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        println!("Program result is {:?}", program);

        match program {
            Ok(program) => {
                let statements_len = program.statements.len();
                println!("Number of statements: {}", statements_len);

                for statement in program.statements {
                    println!("Statement: {:?}", statement);
                }

                if statements_len != 1 {
                    panic!("Expected 1 statement, got {}", statements_len);
                }
            }
            Err(err) => {
                panic!("Failed to parse program: {}", err);
            }
        }
    }

    #[test]
    fn test_parse_multiple_let_statements() {
        let input = String::from("let x = 5; let y = 10;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if program.is_ok() {
            let unwrapped_program = program.unwrap();
            if unwrapped_program.statements.len() != 2 {
                panic!(
                    "Expected 2 statements, got {:?}",
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
