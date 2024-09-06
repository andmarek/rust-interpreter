use crate::token::{Token, TokenType};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) -> String;
}

pub trait Expression: Node {
    fn expression_node(&self) -> String;
}

pub trait Program: Node {
    fn program_node(&self) -> String;
}

pub struct ProgramNode {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for ProgramNode {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }
}

pub enum NodeType {
    StatementType(Box<dyn Statement>),
    ExpressionType(Box<dyn Expression>),
    ProgramType(Box<dyn Program>),
}

impl Node for NodeType {
    fn token_literal(&self) -> String {
        match self {
            NodeType::StatementType(stmt) => stmt.token_literal(),
            NodeType::ExpressionType(expr) => expr.token_literal(),
            NodeType::ProgramType(prog) => prog.token_literal(),
        }
    }
}

pub struct IntegerLiteral {
    pub value: i32,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.value.to_string()
    }
}

pub enum ExpressionType {
    Integer(IntegerLiteral),
}

impl Node for ExpressionType {
    fn token_literal(&self) -> String {
        match self {
            ExpressionType::Integer(i) => i.token_literal(),
        }
    }
}

pub struct Identifier {
    pub name: String,
}

pub struct LetStatement {
    token: Token, // let token
    name: Identifier,
    value: ExpressionType,
}
