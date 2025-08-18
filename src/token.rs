use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenType {
    // Identifiers + Literals
    Ident,
    Int,
    String,

    // Keywords
    Let,
    Function,
    If,
    Else,
    For,
    BooleanTrue,
    BooleanFalse,
    // Operators
    DoubleEqual,
    NotEqual,
    Slash,
    Plus,
    Asterisk,
    Minus,
    // Symbols
    Underscore,
    LessThan,
    GreaterThan,
    Backtick,
    LeftBracket,
    RightBracket,
    LeftParens,
    RightParens,
    Bang,
    Dot,
    Pipe,
    Backslash,
    PoundSign,
    Equals,
    Semicolon,
    Comma,
    LeftBrace,
    RightBrace,
    // Special
    Eof,
    Illegal,
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

static KEYWORDS: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("let", TokenType::Let);
    m.insert("fn", TokenType::Function);
    m.insert("if", TokenType::If);
    m.insert("else", TokenType::Else);
    m
});

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }
}
