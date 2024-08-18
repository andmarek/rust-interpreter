use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum TokenType {
    // Identifiers + Literals
    Ident,
    Int,

    // Keywords
    Let,
    Function,
    If,
    Else,
    // Symbols
    Asterisk,
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
    PlusSign,
    Dot,
    Pipe,
    Backslash,
    PoundSign,

    // Special
    Eof,
    Illegal,
}

#[derive(PartialEq)]
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

    pub fn lookup_ident(ident: &str) -> TokenType {
        KEYWORDS.get(ident).cloned().unwrap_or(TokenType::Ident)
    }
}

pub mod symbols {
    // Weird things
    pub const ILLEGAL: &str = "ILLEGAL";
    pub const EOF: char = '\0';

    //
    pub const POUND_SIGN: char = '#';
    pub const ASTERISK: char = '*';
    pub const UNDERSCORE: char = '_';
    pub const LEFT_ANGLE_BRACKET: char = '<';
    pub const RIGHT_ANGLE_BRACKET: char = '>';
    pub const DASH: char = '-';
    pub const BACKTICK: char = '`';

    pub const LEFT_BRACKET: char = '[';
    pub const RIGHT_BRACKET: char = ']';
    pub const LEFT_PARENS: char = '(';
    pub const RIGHT_PARENS: char = ')';

    pub const EXCLAMATION_MARK: char = '!';
    pub const PLUS_SIGN: char = '+';
    pub const DOT: char = '.';

    pub const PIPE: char = '|';
    pub const BACKSLASH: char = '\\';
}
