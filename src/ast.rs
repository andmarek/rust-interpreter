use crate::token::{Token, TokenType};
/* These are pretty much our "base" types, described from the book. */
pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) -> String;
}

pub trait Expression: Node {
    fn expression_node(&self) -> String;
}

/* A program is the root node of every AST that our parser will generate. A program is
a list of statements. */
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

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>, /* TODO: This should be an identifier I think */
    pub value: Option<Box<dyn Expression>>, /* I have no idea what this is */
}

impl Statement for LetStatement {
    fn statement_node(&self) -> String {
        !unimplemented!()
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

/* Identifier implementation */
pub struct Identifier {
    token: Token,
    value: String,
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
    fn expression_node(&self) -> String {
        !unimplemented!()
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub enum StatementType {
    Let(LetStatement),
}
