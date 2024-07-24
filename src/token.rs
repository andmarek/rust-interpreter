pub mod token {
    #[derive(Debug, PartialEq)]
    pub enum Token {
        IDENT(String),
        INT(String),
        ILLEGAL(char),
        EOF,

        // Operators
        POUND_SIGN,
        ASTERISK,
        UNDERSCORE,
        LEFT_ANGLE_BRAKCET,
        RIGHT_ANGLE_BRACKET,
        DASH,
        BACKTICK,
        LEFT_BRACKET,
        RIGHT_BRACKET,
        LEFT_PARENS,
        RIGHT_PARENS,
        EXCLAMATION_MARK,
        PLUS_SIGN,
        DOT,
        PIPE,
        BACKSLASH,
    }
    pub const POUND_SIGN: char = '#';
    pub const ASTERISK: char = '*';
    pub const UNDERSCORE: char = '_';
    pub const LEFT_ANGLE_BRAKCET: char = '<';
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

    pub struct Token {

    };
}
