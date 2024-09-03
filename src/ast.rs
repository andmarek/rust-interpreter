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
            NodeType::StatementType(stmt) => stmt.statement_node(),
            NodeType::ExpressionType(expr) => expr.expression_node(),
            NodeType::ProgramType(prog) => prog.program_node(),
        }
    }
}

pub struct Identifier {
    pub name: String,
}



pub enum ExpressionType {
    Integer(i32),
}

impl Node for ExpressionType {
    fn token_literal(&self) -> String {
        ExpressionType::Integer(i) => i.token_literal();,
    }
}


pub struct LetStatement {
    token: Token, // let token
    name: Identifier,
    value: ExpressionType,
}
