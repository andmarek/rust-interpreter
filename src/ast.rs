use crate::token::Token;
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub enum ExpressionType {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
}
impl ExpressionType {
    pub fn string(&self) -> String {
        match self {
            ExpressionType::Identifier(id) => id.token.literal.clone(),
            ExpressionType::StringLiteral(sl) => sl.token_literal(),
            ExpressionType::IntegerLiteral(il) => il.token_literal(),
            ExpressionType::PrefixExpression(pe) => pe.token_literal(),
        }
    }
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
    pub statements: Vec<StatementType>,
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
            self.statements[0].token().literal.clone()
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
    pub value: Option<ExpressionType>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<ExpressionType>,
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
    pub token: Token,
    pub value: String,
}

#[derive(Debug)]
pub struct StringLiteral {
    pub value: String,
}

impl StringLiteral {
    pub fn new(value: String) -> StringLiteral {
        StringLiteral { value }
    }
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

#[derive(Debug)]
pub struct NumberLiteral {
    pub token: Token,
    pub value: i64,
}

impl NumberLiteral {
    pub fn new(&mut self, token: Token, value: i64) -> NumberLiteral {
        NumberLiteral { token, value }
    }
}

impl Node for NumberLiteral {
    fn token_literal(&self) -> String {
        self.value.clone().to_string()
    }
    fn string(&self) -> String {
        self.token_literal()
    }
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

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<ExpressionType>,
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

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<ExpressionType>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<ExpressionType>) -> PrefixExpression {
        PrefixExpression {
            token,
            operator,
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.clone().literal
    }
    fn string(&self) -> String {
        let mut str_rep = "".to_owned();
        str_rep.push_str("(");
        str_rep.push_str(self.operator.as_ref());
        str_rep.push_str(&self.right.string());
        str_rep.push_str(")");
        str_rep
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i32,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i32) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.value.clone().to_string()
    }
    fn string(&self) -> String {
        self.token_literal()
    }
}

#[derive(Debug)]
pub enum StatementType {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Integer(IntegerLiteral),
}

impl StatementType {
    pub fn token(&self) -> &Token {
        match self {
            StatementType::Let(s) => &s.token,
            StatementType::Return(s) => &s.token,
            StatementType::Expression(s) => &s.token,
            StatementType::Integer(s) => &s.token,
        }
    }

    pub fn string(&self) -> String {
        match self {
            StatementType::Let(s) => s.string(),
            StatementType::Return(s) => s.string(),
            StatementType::Expression(s) => s.string(),
            StatementType::Integer(s) => s.string(),
        }
    }
}
