use crate::token::token::{Token, *};

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}
impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        return l;
    }

    // Note that the lexer only supports ASCII characters
    // If this were expanded beyond ascii, then characters
    // could be multiple bytes wide, which complicates things.
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input.as_bytes()[self.read_position] as char);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn new_token(token_type: String, ch: char) -> Token {
        return Token {
            token_type,
            literal: ch.to_string(),
        };
    }
    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(ch) = self.ch {
            if ch.is_alphabetic() || ch == '_' {
                identifier.push(ch);
                self.read_char();
            } else {
                break;
            }
        }
        identifier
    }

    fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            Some(ch) => match ch {
                '#' => Lexer::new_token(POUND_SIGN.to_string(), ch),
                '*' => Lexer::new_token(ASTERISK.to_string(), ch),
                '_' => Lexer::new_token(UNDERSCORE.to_string(), ch),
                '<' => Lexer::new_token(LEFT_ANGLE_BRACKET.to_string(), ch),
                '>' => Lexer::new_token(RIGHT_ANGLE_BRACKET.to_string(), ch),
                '-' => Lexer::new_token(DASH.to_string(), ch),
                '`' => Lexer::new_token(BACKTICK.to_string(), ch),
                '[' => Lexer::new_token(LEFT_BRACKET.to_string(), ch),
                ']' => Lexer::new_token(RIGHT_BRACKET.to_string(), ch),
                '(' => Lexer::new_token(LEFT_PARENS.to_string(), ch),
                ')' => Lexer::new_token(RIGHT_PARENS.to_string(), ch),
                '!' => Lexer::new_token(EXCLAMATION_MARK.to_string(), ch),
                '+' => Lexer::new_token(PLUS_SIGN.to_string(), ch),
                '.' => Lexer::new_token(DOT.to_string(), ch),
                '|' => Lexer::new_token(PIPE.to_string(), ch),
                '\\' => Lexer::new_token(BACKSLASH.to_string(), ch),
                _ => Lexer::new_token("Not found".to_string(), ch),
            },
            None => Lexer::new_token(EOF.to_string(), '\0'),
        };
        self.read_char();
        self.read_position += 1;
        return tok;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
    */
    /*
        {token.LET, "let"},
        {token.IDENT, "five"},
        {token.ASSIGN, "="},
        {token.INT, "5"},
        {token.SEMICOLON, ";"},
        {token.LET, "let"},
        {token.IDENT, "ten"},
        {token.ASSIGN, "="},
        {token.INT, "10"},
        {token.SEMICOLON, ";"},
        {token.LET, "let"},
        {token.IDENT, "add"},
        {token.ASSIGN, "="},
        {token.FUNCTION, "fn"},
        {token.LPAREN, "("},
        {token.IDENT, "x"},
        {token.COMMA, ","},
        {token.IDENT, "y"},
        {token.RPAREN, ")"},
        {token.LBRACE, "{"},
        {token.IDENT, "x"},
        {token.PLUS, "+"},
        {token.IDENT, "y"},
        {token.SEMICOLON, ";"},
        {token.RBRACE, "}"},
        {token.SEMICOLON, ";"},
        {token.LET, "let"},
        {token.IDENT, "result"},
        {token.ASSIGN, "="},
        {token.IDENT, "add"},
        {token.LPAREN, "("},
        {token.IDENT, "five"},
        {token.COMMA, ","},
        {token.IDENT, "ten"},
        {token.RPAREN, ")"},
        {token.SEMICOLON, ";"},
        {token.EOF, ""},
    */
    #[test]
    fn test_next_token() {
        let test_string = String::from("this is a test.");
        let mut my_lexer = Lexer::new(test_string);
        assert_eq!(my_lexer.ch, Some('t'));
        my_lexer.read_char();
        assert_eq!(my_lexer.ch, Some('h'));
    }
}
