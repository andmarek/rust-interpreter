use crate::ast::{
    BooleanLiteral, Expression, ExpressionStatement, Identifier, InfixExpression,
    IntegerLiteral, LetStatement, Node, Program, ReturnStatement, StatementType,
    StringLiteral,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use log::debug;
use std::collections::HashMap;
use std::fmt;
use std::vec;

struct ParsingFunctions {
    pub prefix: fn() -> Expression,
    pub infix: fn(Expression) -> Expression,
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
    prefix_parse_fns: HashMap<TokenType, fn(&mut Parser) -> Expression>,
    infix_parse_fns:
        HashMap<TokenType, fn(&mut Parser, Expression) -> Result<Expression, ParseError>>,
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

        // Register prefix parse functions
        parser.register_prefix(TokenType::Ident, Self::parse_identifier);
        parser.register_prefix(TokenType::Int, Self::parse_integer_literal);
        parser.register_prefix(TokenType::Bang, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::BooleanTrue, Self::parse_boolean_expression);
        parser.register_prefix(TokenType::BooleanFalse, Self::parse_boolean_expression);

        // Register infix parse functions
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
    pub fn register_prefix(&mut self, token_type: TokenType, f: fn(&mut Parser) -> Expression) {
        self.prefix_parse_fns.insert(token_type, f);
    }

    /// Adds an infix parsing function for a given token type
    pub fn register_infix(
        &mut self,
        token_type: TokenType,
        f: fn(&mut Parser, Expression) -> Result<Expression, ParseError>,
    ) {
        self.infix_parse_fns.insert(token_type, f);
    }

    /// Main function that gets called to parse the entire program.
    /// A program consists of a list of statements that are parsed one at a time.
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();
        debug!("Beginning to parse the program");

        while let Some(token) = &self.cur_token {
            debug!("Parsing the program, the current token is {:?}", token);

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

    pub fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        // Get current token type
        let token_type = self.cur_token.as_ref()
            .ok_or_else(|| ParseError::InvalidExpression("No current token".into()))?
            .token_type;

        // Get and call prefix parse function
        let prefix_parse_fn = self.prefix_parse_fns.get(&token_type)
            .ok_or_else(|| {
                debug!("No prefix function found for token type {:?}", token_type);
                ParseError::InvalidExpression(format!(
                    "No prefix parse function found for {:?}",
                    token_type
                ))
            })?;

        // Call the prefix parse function for the current token type
        let mut left_exp = prefix_parse_fn(self);

        // While the next token is not a semicolon and the current precedence is higher than the current token
        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence()? {
            // Get the token type of the next token
            let peek_token_type = self.peek_token.as_ref()
                .ok_or_else(|| ParseError::InvalidExpression("No peek token".into()))?
                .token_type;

            // Get the infix parse function for the next token type
            let Some(infix_fn) = self.infix_parse_fns.get(&peek_token_type).copied() else {
                break;
            };

            // Consume the next token
            self.next_token();

            // call the parse_infix_expression for the current (new) token type
            left_exp = infix_fn(self, left_exp)?;
        }

        Ok(left_exp)
    }

    pub fn peek_precedence(&mut self) -> Result<Precedence, String> {
        let tt = self.peek_token.clone().ok_or("err")?.token_type;
        self.precedences(tt)
    }

    pub fn parse_prefix_expression(&mut self) -> Expression {
        let token = match self.cur_token.as_ref() {
            Some(tok) => tok.clone(),
            None => panic!("No current token in parse_prefix_expression"),
        };

        debug!("Parsing prefix expression with token: {:?}", token);
        let operator = token.literal.clone();
        debug!("Operator extracted: {}", operator);

        self.next_token();

        debug!(
            "After moving to next token, current token is: {:?}",
            self.cur_token
        );
        let right = match self.parse_expression(Precedence::Prefix) {
            Ok(result) => {
                debug!("Successfully parsed right expression: {:?}", result);
                Box::new(result)
            }
            Err(err) => {
                debug!("Failed to parse right expression: {:?}", err);
                panic!("Could not parse right expression: {:?}", err)
            }
        };

        let prefix_expr = Expression::Prefix {
            token,
            operator,
            right,
        };
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
        left: Expression,
    ) -> Result<Expression, ParseError> {
        let token = self.cur_token.as_ref().ok_or(ParseError::UnexpectedEOF)?.clone();
        let cur_precedence = self.cur_precedence();
        self.next_token();
        let right = Box::new(self.parse_expression(cur_precedence)?);
        Ok(Expression::Infix {
            token: token.clone(),
            operator: token.literal.clone(),
            left: Box::new(left),
            right,
        })
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
        // Get the token, return ParseError if None
        let token = self.cur_token.clone().ok_or(ParseError::InvalidExpression(
            "No token available".to_string(),
        ))?;

        // Create the statement with the token
        let mut statement = ExpressionStatement {
            token,
            expression: None,
        };

        // Parse the expression and set it in the statement
        // Set the precedence as the lowest since this is the first time we enter the function
        statement.expression = Some(self.parse_expression(Precedence::Lowest)?);

        // Skip any semicolons
        while self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        debug!(
            "Here is the current token at the end of this!: {:?}",
            self.cur_token
        );
        Ok(statement)
    }

    pub fn parse_integer_literal(&mut self) -> Expression {
        let cur_token = match self.cur_token.as_ref() {
            Some(tok) => tok,
            None => panic!("Ahhhh"),
        };
        let integer_value = cur_token.literal.parse::<i64>().unwrap();

        Expression::Integer {
            token: self.cur_token.clone().unwrap(),
            value: integer_value,
        }
    }

    pub fn parse_boolean_expression(&mut self) -> Expression {
        let cur_token = match self.cur_token.as_ref() {
            Some(tok) => tok,
            None => panic!("Ahhhh"),
        };
        let boolean_value = cur_token.token_type == TokenType::BooleanTrue;

        Expression::Boolean {
            token: self.cur_token.clone().unwrap(),
            value: boolean_value,
        }
    }

    pub fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier { token: self.cur_token.clone().unwrap(), value: String::from("") }
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
        return_statement.value = Some(Expression::String{token: self.cur_token.clone().unwrap(), value: ret});
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

        let_statement.value = Some(Expression::String{token: self.cur_token.clone().unwrap(), value: rest});

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

impl InfixExpression {
    pub fn string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.string(),
            self.operator,
            self.right.string()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use once_cell::sync::Lazy;
    use std::sync::Once;

    static INIT: Lazy<()> = Lazy::new(|| {
        let _ = env_logger::builder().is_test(true).try_init();
    });

    fn init_logger() {
        Lazy::force(&INIT);
    }

    // Test helpers
    fn parse_input(program: &str) -> Program {
        let lexer = Lexer::new(program.to_string());
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => program,
            Err(err) => panic!("Failed to parse program: {}", err),
        }
    }

    fn assert_statement_count(program: &Program, expected: usize) {
        if program.statements.len() != expected {
            panic!(
                "Expected {} statements, got {}",
                expected,
                program.statements.len()
            );
        }
    }

    fn assert_statement_literal(program: &Program, index: usize, str_content: &str) {
        if program.statements[index].token().literal != str_content {
            panic!(
                "Expected {} for statement at {}, got {:?}",
                str_content, index, program.statements[index]
            )
        }
    }

    fn extract_prefix_expression(program: &Program) -> &Expression {
        match &program.statements[0] {
            StatementType::Expression(expr_stmt) => match &expr_stmt.expression {
                Some(expr @ Expression::Prefix{ .. }) => expr,
                _ => panic!("Expression is not a prefix expression"),
            },
            _ => panic!("Statement is not an expression statement"),
        }
    }

    #[test]
    fn test_parse_single_let_statement() {
        let program = parse_input("return 5;");
        debug!("Program is {:?}", program.string());
        assert_statement_count(&program, 1);
    }

    #[test]
    fn test_parse_return_statement() {
        let program = parse_input("return 5;");
        assert_statement_count(&program, 1);
    }

    #[test]
    fn test_parse_multiple_let_statements() {
        let input = "let x = 5; let y = 10;";
        let program = parse_input(input);
        assert_statement_count(&program, 2);

        let statements = &program.statements;

        assert_statement_literal(&program, 0, "let");
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
    }

    #[test]
    fn test_parse_let_and_return_statement() {
        let input = "let x = 5; return x;";
        let program = parse_input(input);

        // TODO: need to assert more specifically
        assert_statement_count(&program, 2);
    }

    // #[test]
    // fn test_stringify_program() {
    //     let input = String::from(
    //         "let five = 5;
    //         let ten = 10;
    //         let add = fn(x, y) {
    //         x + y;
    //         };
    //         let result = add(five, ten);
    //         ",
    //     );
    //     let program = parse_input(&input);

    //     debug!("Program is {:?}", program.string());
    //     if program.string()
    //         != "let five = 5;let ten = 10;let add = fn(x,y){x+y;let result = add(five,ten);"
    //     {
    //         panic!("Expected input, got {}", program.string());
    //     }
    // }

    #[test]
    fn test_identifier_expression() {
        let program = parse_input("foobar;");
        assert_statement_count(&program, 1);

        let ref expr_statement = &program.statements[0];
        match expr_statement {
            StatementType::Expression(expr_statement) => {
                assert_eq!(expr_statement.token.literal, "foobar");
            }
            other => panic!("Expected expression statement, got {:?}", other),
        }
    }

    #[test]
    fn test_integer_literal() {
        let program = parse_input("5;");
        assert_statement_count(&program, 1);
        match &program.statements[0] {
            StatementType::Expression(expr_stmt) => {
                if let Some(Expression::Integer{value, ..}) = &expr_stmt.expression {
                    assert_eq!(int_literal.value, 5);
                    assert_eq!(int_literal.token.literal, "5");
                } else {
                    panic!("Expression is not an integer literal");
                }
            }
            _ => panic!("Statement is not an expression statement"),
        }
    }

    #[test]
    pub fn test_parsing_prefix_expression() {
        let inputs = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, expected_operator, expected_value) in inputs.iter() {
            debug!("Testing prefix expression with input: {}", input);
            let program = parse_input(input);
            assert_statement_count(&program, 1);
            let prefix_expr = extract_prefix_expression(&program);

            if let Expression::Prefix { operator, right, ..} = prefix_expr {
                assert_eq!(
                    operator, expected_operator,
                    "Operator mismatch for input '{}'. Expected: '{}', got: '{}'",
                    input, expected_operator, operator
                );
                if let Expression::Integer{ value, ..} = right.as_ref() {
                    assert_eq!(
                        *value, *expected_value,
                        "Value mismatch for input '{}'. Expected: {}, got: {}",
                        input, expected_value, value
                    );
                } else {
                    panic!("Right expression is not an integer literal");
                }
            }

        }
    }

    fn test_identifier(exp: &Expression, value: &str) -> bool {
        match exp {
            Expression::Identifier(ident) => {
                // Check identifier value
                if ident.value != value {
                    println!("ident.value not {}. got={}", value, ident.value);
                    return false;
                }

                // Check token literal
                if ident.token_literal() != value {
                    println!(
                        "ident.token_literal not {}. got={}",
                        value,
                        ident.token_literal()
                    );
                    return false;
                }

                true
            }
            _ => {
                println!("exp not Identifier. got={:?}", exp);
                false
            }
        }
    }

    #[test]
    pub fn test_parse_boolean_expression() {
        let test_programs = [("true;", true), ("false;", false)];
        for (input, expected_value) in test_programs.iter() {
            debug!("Testing boolean expression with input: {}", input);
            let program = parse_input(input);
            assert_statement_count(&program, 1);
            match &program.statements[0] {
                StatementType::Expression(expr_stmt) => match &expr_stmt.expression {
                    Some(Expression::BooleanLiteral(bool_literal)) => {
                        assert_eq!(
                            bool_literal.value, *expected_value,
                            "boolean value not {}. got={}",
                            expected_value, bool_literal.value
                        );
                    }
                    other => panic!("expression is not BooleanLiteral. got={:?}", other),
                },
                other => panic!("statement is not ExpressionStatement. got={:?}", other),
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
            ("5 != 5", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in test_programs.iter() {
            debug!("Testing infix expression with input: {}", input);
            let program = parse_input(input);

            assert_statement_count(&program, 1);

            match &program.statements[0] {
                StatementType::Expression(expr_stmt) => {
                    match &expr_stmt.expression {
                        Some(Expression::InfixExpression(infix)) => {
                            // Check operator
                            assert_eq!(
                                infix.operator, *operator,
                                "operator is not '{}'. got={}",
                                operator, infix.operator
                            );

                            // Check left value
                            match &*infix.left {
                                Expression::IntegerLiteral(left_int) => {
                                    assert_eq!(
                                        left_int.value, *left_value,
                                        "left value not {}. got={}",
                                        left_value, left_int.value
                                    );
                                }
                                other => {
                                    panic!("left expression not IntegerLiteral. got={:?}", other)
                                }
                            }

                            // Check right value
                            match &*infix.right {
                                Expression::IntegerLiteral(right_int) => {
                                    assert_eq!(
                                        right_int.value, *right_value,
                                        "right value not {}. got={}",
                                        right_value, right_int.value
                                    );
                                }
                                other => {
                                    panic!("right expression not IntegerLiteral. got={:?}", other)
                                }
                            }
                        }
                        other => panic!("expression is not InfixExpression. got={:?}", other),
                    }
                }
                other => panic!("statement is not ExpressionStatement. got={:?}", other),
            }
        }
    }

    // on page 82
    #[test]
    pub fn test_operator_precedence_parsing() {
        // page 82
        //init_logger();

        let operator_tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];
        for (input, expected) in operator_tests.iter() {
            let program = parse_input(input);
            let program_str = program.string();

            if &program_str != expected {
                panic!("Expected {}, got {}", expected, program.string())
            }
        }
    }

    #[test]
    pub fn test_boolean_literal() {
        //
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
