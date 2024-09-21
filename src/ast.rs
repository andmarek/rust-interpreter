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
    statements: Vec<Box<dyn Statement>>,
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
    token: Token,
    name: String, /* TODO: This should be an identifier I think */
    value: Box<dyn Expression>,
}
