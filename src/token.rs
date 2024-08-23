use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenType {
    // Identifiers + Literals
    Ident,
    Int,

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
    ExclaimationMarkEquals,
    ForwarSlash,
    PlusSign,
    Asterisk,
    Minus,
    // Symbols
    Underscore,
    LeftAngleBracket,
    RightAngleBracket,
    Dash,
    Backtick,
    LeftBracket,
    RightBracket,
    LeftParens,
    RightParens,
    ExclamationMark,
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
}

#[derive(Debug, PartialEq)]
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
