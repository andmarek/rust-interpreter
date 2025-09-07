use crate::token::Token;
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub enum Expression {
    Identifier { token: Token, value: String },
    Integer { token: Token, value: i64 },
    String { token: Token, value: String },
    Boolean { token: Token, value: bool },
    Prefix { token: Token, operator: String, right: Box<Expression> },
    Infix { token: Token, left: Box<Expression>, operator: String, right: Box<Expression> },
    If { token: Token, condition: Box<Expression>, consequence: Box<BlockStatement>, alternative: Option<Box<BlockStatement>> },
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier { token, .. } => token.literal.clone(),
            Expression::Integer { token, .. } => token.literal.clone(),
            Expression::String { token, .. } => token.literal.clone(),
            Expression::Boolean { token, .. } => token.literal.clone(),
            Expression::Prefix { token, .. } => token.literal.clone(),
            Expression::Infix { token, .. } => token.literal.clone(),
            Expression::If { token, ..} => token.literal.clone(),
        }
    }

    fn string(&self) -> String {
        match self {
            Expression::Identifier { value, .. } => value.clone(),
            Expression::Integer { value, .. } => value.to_string(),
            Expression::String { value, .. } => format!("\"{}\"", value),
            Expression::Boolean { value, .. } => value.to_string(),
            Expression::Prefix { operator, right, .. } => {
                format!("({}{})", operator, right.string())
            },
            Expression::Infix { left, operator, right, .. } => {
                format!("({} {} {})", left.string(), operator, right.string())
            },
            Expression::If { condition, consequence, alternative, .. } => {
                format!("if {} {{ {} }}", condition.string(), consequence.string())
            },
        }
    }
}

pub trait Statement: Node {
    fn statement_node(&self);
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
    pub value: Option<Expression>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Expression>,
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

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
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
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<Expression>) -> PrefixExpression {
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
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        operator: String,
        right: Box<Expression>,
        left: Box<Expression>,
    ) -> InfixExpression {
        InfixExpression {
            token,
            operator,
            right,
            left,
        }
    }

}


impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.clone().literal
    }
    fn string(&self) -> String {
        format!("({} {} {})", self.left.string(), self.operator, self.right.string())
    }

}

#[derive(Debug)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> BooleanLiteral {
        BooleanLiteral { token, value }
    }
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> String {
        self.value.clone().to_string()
    }
    fn string(&self) -> String {
        self.token_literal()
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
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.clone().literal
    }
    fn string(&self) -> String {
        let mut result = String::new();
        for statement in &self.statements {
            result.push_str(&statement.string());
        }
        result
    }
}

impl Statement for IntegerLiteral {
    fn statement_node(&self) {
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

    pub fn into_statement(self) -> Box<dyn Statement> {
        match self {
            StatementType::Let(stmt) => Box::new(stmt),
            StatementType::Return(stmt) => Box::new(stmt),
            StatementType::Expression(stmt) => Box::new(stmt),
            StatementType::Integer(stmt) => Box::new(stmt),
        }
    }
}
