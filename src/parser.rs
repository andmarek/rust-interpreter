use std::fmt;
use crate::ast::{
    Expression, ExpressionStatement, ExpressionType, Identifier, InfixExpression, IntegerLiteral,
    LetStatement, Node, PrefixExpression, Program, ReturnStatement, StatementType, StringLiteral,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::HashMap;
use std::vec;
use log::debug;

struct ParsingFunctions {
    pub prefix: fn() -> ExpressionType,

    pub infix: fn(ExpressionType) -> ExpressionType,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    InvalidExpression(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedEOF => write!(f, "Unexpected end of file"),
            ParseError::InvalidExpression(msg) => write!(f, "Invalid expression: {}", msg),
        }
    }
}


impl From<String> for ParseError {
    fn from(error: String) -> Self {
        ParseError::InvalidExpression(error)
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    // Each token we encounter can one of two types of functions associated with parsing it,
    // depending on whether the token is found in the infix or prefix position
    prefix_parse_fns: HashMap<TokenType, fn(&mut Parser) -> ExpressionType>,
    infix_parse_fns:
        HashMap<TokenType, fn(&mut Parser, ExpressionType) -> Result<ExpressionType, ParseError>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // myFunction(X)
    Index = 7,       // array[index]
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        debug!("{:?}", lexer.get_input());
        let mut parser = Parser {
            lexer,
            cur_token: None,
            peek_token: None,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        // Register parse functions for each type of expression
        parser.register_prefix(TokenType::Ident, Self::parse_identifier);
        parser.register_prefix(TokenType::Int, Self::parse_integer_literal);
        parser.register_prefix(TokenType::Bang, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Self::parse_prefix_expression);

        parser.register_infix(TokenType::Plus, Self::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Self::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Self::parse_infix_expression);
        parser.register_infix(TokenType::GreaterThan, Self::parse_infix_expression);
        parser.register_infix(TokenType::LessThan, Self::parse_infix_expression);
        parser.register_infix(TokenType::DoubleEqual, Self::parse_infix_expression);
        parser.register_infix(TokenType::NotEqual, Self::parse_infix_expression);

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());

        debug!("cur_token is {:?}", self.cur_token);
        debug!(
            "peek_token is {:?}",
            self.peek_token.clone().unwrap().literal
        );
    }

    pub fn get_errors(self) -> Vec<String> {
        return self.errors.clone();
    }

    /// Adds a prefix parsing function for a given token type
    pub fn register_prefix(&mut self, token_type: TokenType, f: fn(&mut Parser) -> ExpressionType) {
        self.prefix_parse_fns.insert(token_type, f);
    }

    /// Adds an infix parsing function for a given token type
    pub fn register_infix(
        &mut self,
        token_type: TokenType,
        f: fn(&mut Parser, ExpressionType) -> Result<ExpressionType, ParseError>,
    ) {
        self.infix_parse_fns.insert(token_type, f);
    }

    /// Main function that gets called to parse the entire program.
    /// A program consists of a list of statements that are parsed one at a time.
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();

        debug!("Beginning to parse the program");

        // Single while loop
        while let Some(token) = &self.cur_token {
            debug!(
                "----------Parsing the program, the current token is {:?}",
                token
            );

            match self.parse_statement()? {
                Some(statement) => program.statements.push(statement),
                None => break,
            }

            self.next_token();
        }

        debug!("Program statements: {:?}", program.statements);
        debug!("We're done parsing the program.");
        Ok(program)
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<ExpressionType, ParseError> {
        let token_type = self
            .cur_token
            .as_ref()
            .ok_or(ParseError::InvalidExpression(String::from("No current token")))?
            .token_type;

        let prefix_fn = match self.prefix_parse_fns.get(&token_type) {
            Some(func) => *func,
            None => {
                debug!("No prefix function found for token type {:?}", token_type);
                return Err(ParseError::InvalidExpression(format!(
                    "No prefix parse function found for {:?}",
                    token_type
                )));
            }
        };
        
        let mut left_exp = prefix_fn(self);

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence()? {
            let peek_token_type = self.peek_token.as_ref().ok_or(ParseError::InvalidExpression(String::from("No peek token")))?.token_type;

            // Clone the function pointer outside the match
            let infix_fn = match self.infix_parse_fns.get(&peek_token_type).copied() {
                Some(f) => f,
                None => break,
            };

            // Now we can mutably borrow self
            self.next_token();
            left_exp = infix_fn(self, left_exp)?;
        }

        Ok(left_exp)
    }

    pub fn peek_precedence(&mut self) -> Result<Precedence, String> {
        let tt = self.peek_token.clone().ok_or("err")?.token_type;
        self.precedences(tt)
    }

    pub fn parse_prefix_expression(&mut self) -> ExpressionType {
        let token = match self.cur_token.as_ref() {
            Some(tok) => tok.clone(),
            None => panic!("No current token in parse_prefix_expression"),
        };
        
        debug!("Parsing prefix expression with token: {:?}", token);
        let operator = token.literal.clone();
        debug!("Operator extracted: {}", operator);
        
        self.next_token();
        
        debug!("After moving to next token, current token is: {:?}", self.cur_token);
        let right = match self.parse_expression(Precedence::Prefix) {
            Ok(result) => {
                debug!("Successfully parsed right expression: {:?}", result);
                Box::new(result)
            },
            Err(err) => {
                debug!("Failed to parse right expression: {:?}", err);
                panic!("Could not parse right expression: {:?}", err)
            },
        };
        
        let prefix_expr = ExpressionType::PrefixExpression(PrefixExpression {
            token,
            operator,
            right,
        });
        debug!("Created prefix expression: {:?}", prefix_expr);
        prefix_expr
    }

    pub fn precedences(&mut self, t: TokenType) -> Result<Precedence, String> {
        Ok(match t {
            TokenType::Equals => Precedence::Equals,
            TokenType::NotEqual => Precedence::Equals,
            TokenType::DoubleEqual => Precedence::Equals,
            TokenType::LessThan => Precedence::LessGreater,
            TokenType::GreaterThan => Precedence::LessGreater,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Precedence::Sum,
            TokenType::Slash => Precedence::Product,
            TokenType::Asterisk => Precedence::Product,
            TokenType::LeftParens => Precedence::Call,
            TokenType::LeftBracket => Precedence::Index,
            _ => Precedence::Lowest,
        })
    }

    pub fn cur_precedence(&mut self) -> Precedence {
        let token = match self.cur_token.as_ref() {
            Some(tok) => tok.clone(),
            None => return Precedence::Lowest,
        };

        self.precedences(token.token_type)
            .unwrap_or(Precedence::Lowest)
    }

    pub fn parse_infix_expression(
        &mut self,
        left: ExpressionType,
    ) -> Result<ExpressionType, ParseError> {
        let token = self
            .cur_token
            .as_ref()
            .ok_or(ParseError::UnexpectedEOF)?
            .clone();
        let operator = token.literal.clone();
        let cur_precedence = self.cur_precedence();
        self.next_token();
        let right = Box::new(self.parse_expression(cur_precedence)?);
        Ok(ExpressionType::InfixExpression(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right,
        }))
    }

    pub fn parse_statement(&mut self) -> Result<Option<StatementType>, ParseError> {
        debug!("Parsing statement");
        match &self.cur_token {
            Some(token) => {
                let stmt = match token.token_type {
                    TokenType::Let => StatementType::Let(self.parse_let_statement()?),
                    TokenType::Return => StatementType::Return(self.parse_return_statement()?),
                    TokenType::Eof => return Ok(None),
                    _ => StatementType::Expression(self.parse_expression_statement()?),
                };
                Ok(Some(stmt))
            }
            None => Ok(None),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        debug!("EXPRESSION statement, cur_token: {:?}", self.cur_token);

        // Get the token, return ParseError if None
        let token = self.cur_token.clone()
            .ok_or(ParseError::InvalidExpression("No token available".to_string()))?;

        // Create the statement with the token
        let mut statement = ExpressionStatement {
            token,
            expression: None,
        };

        // Parse the expression and set it in the statement
        statement.expression = Some(self.parse_expression(Precedence::Lowest)?);

        // Skip any semicolons
        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        debug!("Here is the current token at the end of this!: {:?}", self.cur_token);
        Ok(statement)
    }

    pub fn parse_integer_literal(&mut self) -> ExpressionType {
        let cur_token = match self.cur_token.as_ref() {
            Some(tok) => tok,
            None => panic!("Ahhhh"),
        };
        let integer_value = cur_token.literal.parse::<i32>().unwrap();

        ExpressionType::IntegerLiteral(IntegerLiteral {
            token: self.cur_token.clone().unwrap(),
            value: integer_value,
        })
    }
    /*
    pub fn parse_expression(&mut self) -> Result<ExpressionStatement, String> {
        while !self.cur_token_is(TokenType::Semicolon) {
            self.parse_identifier();
        }
    }
    */

    pub fn parse_identifier(&mut self) -> ExpressionType {
        ExpressionType::Identifier(Identifier::new(self.cur_token.clone().unwrap()))
    }

    /// Parses a return statement of the form
    /// return <expression>;
    /// For now, we'll just implement it like parsing until a ;
    pub fn parse_return_statement(&mut self) -> Result<ReturnStatement, String> {
        debug!("Parsing return statement");
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
        debug!("Testing with input: {}", input); // This will print

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(program) => {
                debug!("Program result is {:?}", program.string());
                let statements_len = program.statements.len();
                debug!("Number of statements: {}", statements_len);
                for statement in program.statements {
                    debug!("Statement: {:?}", statement);
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
                debug!("Program is {:?}", program.string());
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
                    panic!(
                        "Expected first statement to be let, got {}",
                        statements[0].token().literal
                    );
                }
                match &statements[0] {
                    StatementType::Let(let_stmt) => {
                        if let Some(name) = &let_stmt.name {
                            if name.token.literal != "x" {
                                panic!("Expected name to be x, got {}", name.token.literal);
                            }
                        }
                    }
                    _ => panic!(
                        "Expected first statement to be a let statement, got {:?}",
                        statements[0]
                    ),
                }

                if statements[1].token().literal != "let" {
                    panic!(
                        "Expected second statement to be let, got {}",
                        statements[1].token().literal
                    );
                }
            }
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
                    panic!("Expected 2 statements, got {:?}", statements.len());
                }
            }
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

            debug!("Program is {:?}", unwrapped_program.string());
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
            }
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

    #[test]
    fn test_integer_literal() {
        let l = Lexer::new(String::from("5;"));
        let mut p = Parser::new(l);

        match p.parse_program() {
            Ok(program) => {
                if program.statements.len() != 1 {
                    panic!(
                        "program has not enough statements, got {:?}",
                        program.statements.len()
                    )
                }
                match &program.statements[0] {
                    StatementType::Expression(expr_stmt) => {
                        if let Some(ExpressionType::IntegerLiteral(int_literal)) =
                            &expr_stmt.expression
                        {
                            assert_eq!(int_literal.value, 5);
                            assert_eq!(int_literal.token.literal, "5");
                        } else {
                            panic!("Expression is not an integer literal");
                        }
                    }
                    _ => panic!("Statement is not an expression statement"),
                }
            }
            Err(err) => panic!("Parser error: {}", err),
        }
    }

    /// Helper function to parse programs. Currently for tests.
    fn parse_test_program(input: &str) -> Program {
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        p.parse_program().unwrap_or_else(|err| {
            panic!("Failed to parse program: {}", err);
        })
    }

    ///
    fn extract_prefix_expression(program: &Program) -> &PrefixExpression {
        assert_eq!(
            program.statements.len(),
            1,
            "Program should have exactly one statement"
        );
        match &program.statements[0] {
            StatementType::Expression(expr_stmt) => match &expr_stmt.expression {
                Some(ExpressionType::PrefixExpression(prefix_expr)) => prefix_expr,
                _ => panic!("Expression is not a prefix expression"),
            },
            _ => panic!("Statement is not an expression statement"),
        }
    }

    #[test]
    pub fn test_parsing_prefix_expression() {
        let test_data = [("!5;", "!", 5), ("-15;", "-", 15)];
        for (input, operator, expected_value) in test_data.iter() {
            debug!("Testing prefix expression with input: {}", input);
            let program = parse_test_program(input);
            let prefix_expr = extract_prefix_expression(&program);

            assert_eq!(
                prefix_expr.operator, 
                *operator,
                "Operator mismatch for input '{}'. Expected: '{}', got: '{}'",
                input, operator, prefix_expr.operator
            );

            match &*prefix_expr.right {
                ExpressionType::IntegerLiteral(int) => {
                    assert_eq!(
                        int.value, 
                        *expected_value,
                        "Value mismatch for input '{}'. Expected: {}, got: {}",
                        input, expected_value, int.value
                    );
                }
                other => panic!(
                    "Expected integer literal for input '{}', got {:?}",
                    input, other
                ),
            }
        }
    }

    #[test]
    pub fn test_parse_infix_expression() {
        let test_programs = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            //("5 != 5", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in test_programs.iter() {
            debug!("Testing infix expression with input: {}", input);
            let program = parse_test_program(input);
            
            assert_eq!(
                program.statements.len(), 
                1,
                "program should have exactly one statement. got={}",
                program.statements.len()
            );

            match &program.statements[0] {
                StatementType::Expression(expr_stmt) => {
                    match &expr_stmt.expression {
                        Some(ExpressionType::InfixExpression(infix)) => {
                            // Check operator
                            assert_eq!(
                                infix.operator, 
                                *operator,
                                "operator is not '{}'. got={}",
                                operator, 
                                infix.operator
                            );

                            // Check left value
                            match &*infix.left {
                                ExpressionType::IntegerLiteral(left_int) => {
                                    assert_eq!(
                                        left_int.value, 
                                        *left_value,
                                        "left value not {}. got={}",
                                        left_value, 
                                        left_int.value
                                    );
                                },
                                other => panic!("left expression not IntegerLiteral. got={:?}", other),
                            }

                            // Check right value
                            match &*infix.right {
                                ExpressionType::IntegerLiteral(right_int) => {
                                    assert_eq!(
                                        right_int.value, 
                                        *right_value,
                                        "right value not {}. got={}",
                                        right_value, 
                                        right_int.value
                                    );
                                },
                                other => panic!("right expression not IntegerLiteral. got={:?}", other),
                            }
                        },
                        other => panic!("expression is not InfixExpression. got={:?}", other),
                    }
                },
                other => panic!("statement is not ExpressionStatement. got={:?}", other),
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
