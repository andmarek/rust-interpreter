use crate::token::{Token, TokenType};
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node + std::fmt::Debug {
    fn expression_node(&self);
}

/* A program is the root node of every AST that our parser will generate. A program is
a list of statements. */
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl fmt::Debug for dyn Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.token_literal())
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }

    fn string(&self) -> String {
        let mut out = String::new();
        for stmt in &self.statements {
            out.push_str(&stmt.string());
        }
        out
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Box<dyn Expression>>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str(" ");
        if let Some(value) = &self.value {
            out.push_str(&value.string());
        }
        out
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str(" ");

        if let Some(name) = &self.name {
            out.push_str(&name.string());
        }

        out.push_str(" = ");

        if let Some(value) = &self.value {
            out.push_str(&value.string());
        }
        return out;
    }
}

/* Identifier implementation */
#[derive(Debug)]
pub struct Identifier {
    token: Token,
    value: String,
}

#[derive(Debug)]
pub struct StringLiteral {
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.value.clone()
    }
    fn string(&self) -> String {
        return format!("{}", self.token_literal());
    }
}

impl Expression for StringLiteral {
    fn expression_node(&self) {}
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        Identifier {
            token,
            value: String::from(""),
        }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        if let Some(expr) = &self.expression {
            return expr.string();
        } else {
            return String::from("");
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn string(&self) -> String {
        return format!("{}", self.token_literal());
    }
}

pub enum StatementType {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}
