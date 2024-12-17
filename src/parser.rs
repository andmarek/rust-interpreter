use crate::ast::{
    Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement,
    StatementType, StringLiteral, ExpressionType, Node
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::HashMap;
use std::vec;

struct ParsingFunctions {
    pub prefix: fn() -> Result<ExpressionType, String>,
    pub infix: fn(ExpressionType) -> Result<ExpressionType, String>,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    // Each token we encounter can one of two types of functions associated with parsing it,
    // depending on whether the token is found in the infix or prefix position
    prefix_parse_fns: HashMap<TokenType, fn() -> Result<ExpressionType, String>>,
    infix_parse_fns: HashMap<TokenType, fn(ExpressionType) -> Result<ExpressionType, String>>,
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

    pub fn get_errors(self) -> Vec<String> {
        return self.errors.clone();
    }

    /// Adds a prefix parsing function for a given token type
    pub fn register_prefix(&mut self, token_type: TokenType, f: fn() -> Result<ExpressionType, String>) {
        self.prefix_parse_fns.insert(token_type, f);
    }

    /// Adds an infix parsing function for a given token type
    pub fn register_infix(
        &mut self,
        token_type: TokenType,
        f: fn(ExpressionType) -> Result<ExpressionType, String>,
    ) {
        self.infix_parse_fns.insert(token_type, f);
    }

    /// Main function that gets called to parse the entire program.
    /// A program consists of a list of statements that are parsed one at a time.
    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program::new();

        println!("Beginning to parse the program");

        // Single while loop
        while let Some(token) = &self.cur_token {
            println!("----------Parsing the program, the current token is {:?}", token);

            match self.parse_statement()? {
                Some(statement) => program.statements.push(statement),
                None => break,
            }

            self.next_token();
        }

        print!("Program statements: {:?}", program.statements);
        println!("We're done parsing the program.");
        Ok(program)
    }
    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<ExpressionType, String> {
        let token_type = match &self.cur_token {
            Some(token) => &token.token_type,
            None => return Err("No current token".to_string()),
        };
        let prefix = self.prefix_parse_fns.get(token_type);
        match &prefix {
            Some(f) => f(),
            None => return Err(format!("No prefix parsing function for token type {:?}", token_type)),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Option<StatementType>, String> {
        println!("Parsing statement");
        match &self.cur_token {
            Some(token) => {
                let stmt = match token.token_type {
                    TokenType::Let => StatementType::Let(self.parse_let_statement()?),
                    TokenType::Return => StatementType::Return(self.parse_return_statement()?),
                    TokenType::Eof => return Ok(None),
                    _ => StatementType::Expression(self.parse_expression()?),
                };
                Ok(Some(stmt))
            }
            None => Ok(None),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, String> {
        println!("EXPRESSION statement, cur_token: {:?}", self.cur_token);

        let mut statement = ExpressionStatement {
            token: self.cur_token.clone().ok_or("No current token")?,
            expression: None,
        };

        statement.expression = match self.parse_expression(Precedence::Lowest) {
            Ok(expr) => Some(expr),
            Err(err) => return Err(err),
        };

        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        println!(
            "Here is the current token at the end of this!: {:?}",
            self.cur_token
        );

        return Ok(statement);
    }

    /*
    pub fn parse_expression(&mut self) -> Result<ExpressionStatement, String> {
        while !self.cur_token_is(TokenType::Semicolon) {
            self.parse_identifier();
        }
    }
    */

    pub fn parse_identifier(&mut self) -> Identifier {
        Identifier::new(self.cur_token.clone().unwrap())
    }

    /// Parses a return statement of the form
    /// return <expression>;
    /// For now, we'll just implement it like parsing until a ;
    pub fn parse_return_statement(&mut self) -> Result<ReturnStatement, String> {
        println!("Parsing return statement");
        let cur_token = self.cur_token.clone().ok_or("No current token")?;
        let mut return_statement = ReturnStatement {
            token: cur_token,
            value: None,
        };
        self.next_token();
        let mut ret = String::new();
        while !self.cur_token_is(TokenType::Semicolon) {
            ret.push_str(&self.cur_token.clone().unwrap().literal);
            self.next_token();
        }
        return_statement.value = Some(ExpressionType::StringLiteral(StringLiteral::new(ret)));
        Ok(return_statement)
    }

    pub fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        // TODO: do we actually have to clone this?
        let token = self.cur_token.clone().ok_or("No current token")?;

        let mut let_statement = LetStatement {
            token,
            name: None,
            value: None,
        };

        /* If we don't have an identifier, then it's not a valid let statement */
        if !self.expect_peek(TokenType::Ident) {
            return Err("Expected identifier".to_string());
        }

        let identifier = self.cur_token.clone().ok_or("No current token")?;
        let_statement.name = Some(Identifier::new(identifier));

        /* TODO: Change EQUALS to ASSIGN */
        if !self.expect_peek(TokenType::Equals) {
            return Err("Expected equals sign after identifier in let statement".to_string());
        }

        /* TODO: eventually replace this with parse expression */
        let mut rest = String::new();
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
            rest.push_str(&self.cur_token.clone().unwrap().literal);
        }

        let_statement.value = Some(ExpressionType::StringLiteral(StringLiteral::new(rest)));

        Ok(let_statement)
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

        match parser.parse_program() {
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
        match program {
            Ok(program) => {
                let statements = program.statements;
                if statements.len() != 2 {
                    panic!("Expected 2 statements, got {:?}", statements.len());
                }

                if statements[0].token().literal != "let" {
                    panic!("Expected first statement to be let, got {}", statements[0].token().literal);
                }
                match &statements[0] {
                    StatementType::Let(let_stmt) => {
                        if let Some(name) = &let_stmt.name {
                            if name.token.literal != "x" {
                                panic!("Expected name to be x, got {}", name.token.literal);
                            }
                        }
                    },
                    _ => panic!("Expected first statement to be a let statement, got {:?}", statements[0]),
                }

                if statements[1].token().literal != "let" {
                    panic!("Expected second statement to be let, got {}", statements[1].token().literal);
                }
            },
            Err(err) => {
                panic!("Failed to parse program: {}", err);
            }
        }
    }

    #[test]
    fn test_parse_let_and_return_statement() {
        let input = String::from("let x = 5; return x;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        match program {
            Ok(program) => {
                let statements = program.statements;
                if statements.len() != 2 {
                    panic!(
                        "Expected 2 statements, got {:?}",
                        statements.len()
                    );
                }
            },
            Err(err) => {
                panic!("Failed to parse program: {}", err);
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
        match parser.parse_program() {
            Ok(program) => {
                if program.statements.len() != 1 {
                    panic!("Expected 1 statement, got {}", program.statements.len());
                }
                let ref expr_statement = &program.statements[0];

                match expr_statement {
                    StatementType::Expression(expr_statement) => {
                        assert_eq!(expr_statement.token.literal, "foobar");
                    }
                    other => panic!("Expected expression statement, got {:?}", other),
                }
            },
            Err(err) => {
                panic!("Parser error: {}", err);
            }
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
