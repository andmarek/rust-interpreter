use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, Node, Program, ReturnStatement,
    Statement, StatementType, StringLiteral,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::any::Any;
use std::collections::HashMap;
use std::vec;

struct ParsingFunctions {
    pub prefix: fn() -> Box<dyn Expression>,
    pub infix: fn(Box<dyn Expression>) -> Box<dyn Expression>,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, fn() -> Box<dyn Expression>>,
    infix_parse_fns: HashMap<TokenType, fn(Box<dyn Expression>) -> Box<dyn Expression>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Precedence {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // myFunction(X)
    Index = 7,       // array[index]
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        println!("{:?}", lexer.get_input());
        let mut parser = Parser {
            lexer,
            cur_token: None,
            peek_token: None,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
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

    /*
    pub fn parse_identifier(&mut self) -> Box<dyn Expression> {
        Box::new(Identifier {
            token: self.cur_token.as_ref().unwrap(),
            value: self.cur_token.as_ref().unwrap().literal,
        })
    }
    */

    pub fn get_errors(self) -> Vec<String> {
        return self.errors.clone();
    }

    pub fn register_prefix(&mut self, token_type: TokenType, f: fn() -> Box<dyn Expression>) {
        self.prefix_parse_fns.insert(token_type, f);
    }

    pub fn register_infix(
        &mut self,
        token_type: TokenType,
        f: fn(Box<dyn Expression>) -> Box<dyn Expression>,
    ) {
        self.infix_parse_fns.insert(token_type, f);
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program::new();

        println!("Beginning to parse the program");

        while let Some(token) = &self.cur_token {
            println!(
                "----------Parsing the program, the current token is {:?}",
                token
            );

            let statement = self.parse_statement();

            let boxed_statement: Box<dyn Statement> = match statement {
                Some(Ok(StatementType::Let(let_stmt))) => Box::new(let_stmt),
                Some(Ok(StatementType::Return(return_stmt))) => Box::new(return_stmt),
                Some(Ok(StatementType::Expression(expression_stmt))) => Box::new(expression_stmt),
                Some(Err(err)) => return Err(err),
                None => break, // None kinda implies EOF here, should change that.
            };

            println!("Statement is {:?}", boxed_statement);

            program.statements.push(boxed_statement);

            self.next_token();
        }
        print!("Program statements: {:?}", program.statements);
        println!("We're done parsing the program.");
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> Option<Result<StatementType, String>> {
        println!("Parsing statement");
        match &self.cur_token {
            Some(token) => match token.token_type {
                TokenType::Eof => {
                    println!("Encountered EOF");
                    None
                }
                TokenType::Let => {
                    println!("Parsing Let statement");
                    Some(self.parse_let_statement().map(StatementType::Let))
                }
                TokenType::Return => {
                    println!("Parsing Return statement");
                    Some(self.parse_return_statement().map(StatementType::Return))
                }
                _ => {
                    println!("Parsing Expression statement");
                    Some(
                        self.parse_expression_statement()
                            .map(StatementType::Expression),
                    )
                }
            },
            None => None,
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, String> {
        println!("EXPRESSION statement, cur_token: {:?}", self.cur_token);

        let statement = ExpressionStatement {
            token: self.cur_token.clone().ok_or("No current token")?,
            expression: None,
        };

        // skip the rest of the tokens until semicolon
        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        //statement.expression = self.parse_expression(LOWEST);

        println!(
            "Here is the current token at the end of this!: {:?}",
            self.cur_token
        );

        return Ok(statement);
    }

    pub fn parse_expression(&mut self) -> Option<Box<dyn Expression>> {
        match &self.cur_token {
            Some(token) => match token.token_type {
                TokenType::Ident => Some(Box::new(Identifier::new(token.clone()))),
                _ => {
                    if let Some(prefix_fn) = self.prefix_parse_fns.get(&token.token_type) {
                        Some(prefix_fn())
                    } else {
                        None
                    }
                }
            },
            None => None,
        }
    }

    pub fn parse_return_statement(&mut self) -> Result<ReturnStatement, String> {
        let cur_token = self.cur_token.clone().ok_or("No current token")?;
        let mut statement = ReturnStatement {
            token: cur_token,
            value: None,
        };

        // TODO: expression parsing is not implemented yet,
        // so we're skipping over the expression for now.
        let mut rest = String::new();
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
            rest.push_str(&self.cur_token.clone().unwrap().literal);
        }
        statement.value = Some(Box::new(StringLiteral { value: rest }));

        Ok(statement)
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

        let identifier = self.cur_token.clone().ok_or("No current token")?;
        statement.name = Some(Identifier::new(identifier));

        /* TODO: Change EQUALS to ASSIGN */

        if !self.expect_peek(TokenType::Equals) {
            return Err("Expected equals sign after identifier in let statement".to_string());
        }
        let mut rest = String::new();
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
            rest.push_str(&self.cur_token.clone().unwrap().literal);
        }
        statement.value = Some(Box::new(StringLiteral { value: rest }));
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

    pub fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "Expected next token to be {}, got {}",
            token.literal,
            self.peek_token.as_ref().unwrap().literal
        );
        self.errors.push(msg);
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

        match program {
            Ok(program) => {
                println!("Program result is {:?}", program.string());
                let statements_len = program.statements.len();
                println!("Number of statements: {}", statements_len);
                for statement in program.statements {
                    println!("Statement: {:?}", statement);
                }

                if statements_len != 1 {
                    panic!("Expected 1 statement, got {}", statements_len);
                }
                //panic!();
            }
            Err(err) => {
                panic!("Failed to parse program: {}", err);
            }
        }
    }

    #[test]
    fn test_parse_return_statement() {
        let input = String::from("return 5;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        match program {
            Ok(program) => {
                println!("Program is {:?}", program.string());
                let unwrapped_program = program;
                if unwrapped_program.statements.len() != 1 {
                    panic!(
                        "Expected 1 statement, got {}",
                        unwrapped_program.statements.len()
                    );
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

    #[test]
    fn test_parse_let_and_return_statement() {
        let input = String::from("let x = 5; return x;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.is_ok() {
            let unwrapped_program = program.unwrap();

            println!("Program is {:?}", unwrapped_program.statements[0].string());
            if unwrapped_program.statements[0].string() != "let x = 5;" {
                panic!(
                    "Expected let statement, got {}",
                    unwrapped_program.statements[0].string()
                );
            }
            if unwrapped_program.statements[1].string() != "return x;" {
                panic!(
                    "Expected return statement, got {}",
                    unwrapped_program.statements[1].string()
                );
            }

            if unwrapped_program.statements.len() != 2 {
                panic!(
                    "Expected 2 statements, got {:?}",
                    unwrapped_program.statements.len()
                );
            }
        }
    }

    #[test]
    fn test_stringify_program() {
        let input = String::from(
            "let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            ",
        );
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if program.is_ok() {
            let unwrapped_program = program.unwrap();

            println!("Program is {:?}", unwrapped_program.string());
            if unwrapped_program.string()
                != "let five = 5;let ten = 10;let add = fn(x,y){x+y;let result = add(five,ten);"
            {
                panic!("Expected input, got {}", unwrapped_program.string());
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        // Test the parsing directly
        match parser.parse_statement() {
            Some(Ok(StatementType::Expression(expr_statement))) => {
                assert_eq!(expr_statement.token.literal, "foobar");
            }
            Some(Ok(other)) => panic!("Expected expression statement, got {:?}", other),
            Some(Err(err)) => panic!("Parser error: {}", err),
            None => panic!("Expected Some statement, got None"),
        }
    }

    fn check_errors(p: &Parser) {
        if p.errors.len() == 0 {
            return;
        }
        panic!("There were some errors in the parsing: {:?}", p.errors);
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
