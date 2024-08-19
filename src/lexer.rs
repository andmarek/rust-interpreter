use crate::token::{
    symbols::{self, PIPE},
    Token, TokenType,
};

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
        // why read char first?
        l.read_char();
        l
    }

    // Note that the lexer only supports ASCII characters
    // If this were expanded beyond ASCII, then characters
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

    fn new_token(token_type: TokenType, ch: char) -> Token {
        // Takes token type and char and returns a Token obj with literal
        Token {
            token_type,
            literal: ch.to_string(),
        }
    }

    fn read_identifier(&mut self) -> String {
        let start_position = self.position;
        while let Some(ch) = self.ch {
            if ch.is_alphabetic() || ch == '_' {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[start_position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let start_position = self.position;
        while let Some(ch) = self.ch {
            if ch.is_digit(10) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[start_position..self.position].to_string()
    }

    fn eat_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if ch.is_whitespace() {
                self.read_char()
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.eat_whitespace();
        let tok = match self.ch {
            Some(ch) => match ch {
                '#' => Lexer::new_token(TokenType::PoundSign, ch),
                '*' => Lexer::new_token(TokenType::Asterisk, ch),
                '_' => Lexer::new_token(TokenType::Underscore, ch),
                '<' => Lexer::new_token(TokenType::LeftAngleBracket, ch),
                '>' => Lexer::new_token(TokenType::RightAngleBracket, ch),
                '-' => Lexer::new_token(TokenType::Dash, ch),
                '`' => Lexer::new_token(TokenType::Backtick, ch),
                '[' => Lexer::new_token(TokenType::LeftBracket, ch),
                ']' => Lexer::new_token(TokenType::RightBracket, ch),
                '(' => Lexer::new_token(TokenType::LeftParens, ch),
                ')' => Lexer::new_token(TokenType::RightParens, ch),
                '!' => Lexer::new_token(TokenType::ExclamationMark, ch),
                '+' => Lexer::new_token(TokenType::PlusSign, ch),
                '.' => Lexer::new_token(TokenType::Dot, ch),
                '|' => Lexer::new_token(TokenType::Pipe, ch),
                '=' => Lexer::new_token(TokenType::Equals, ch),
                '\\' => Lexer::new_token(TokenType::Backslash, ch),
                // this is where we'd read the identifier or keyword I think
                _ => {
                    // if we've exhausted our current symbols and we still have an alphabetic character
                    if ch.is_alphabetic() {
                        let ident = self.read_identifier();
                        // returns the keyword or the identifier
                        Token::new(Token::lookup_ident(ident.as_str()), ident)
                    } else if ch.is_digit(10) {
                        Token::new(TokenType::Int, self.read_number())
                    } else {
                        Lexer::new_token(TokenType::Illegal, ch)
                    }
                }
            },
            None => Lexer::new_token(TokenType::Eof, '\0'),
        };
        self.read_char();
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
        let input = String::from(
            "let five = 5;
             let ten = 10;
               let add = fn(x, y) {
                 x + y;
            };
               let result = add(five, ten);
            ",
        );
        let mut my_lexer = Lexer::new(input);
        assert_eq!(
            my_lexer.next_token(),
            Token::new(TokenType::Let, String::from("let"))
        );
        assert_eq!(
            my_lexer.next_token(),
            Token::new(TokenType::Ident, String::from("five"))
        );
        assert_eq!(
            my_lexer.next_token(),
            Token::new(TokenType::Equals, String::from("="))
        );
    }
}
