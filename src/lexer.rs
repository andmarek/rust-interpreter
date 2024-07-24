use token;

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<u8>,
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
            self.ch = Some(self.input.as_bytes()[self.read_position]);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> token::Token {
        let token = match self.ch {
            token::IDENT => "/",
            token::IDENT(String) =>,
            token::INT(String) => "",
            token::ILLEGAL(char) => "",
            token::EOF => "",

            token::// Operators
            token::POUND_SIGN => "",
            token::ASTERISK => "",
            token::UNDERSCORE => "",
            token::LEFT_ANGLE_BRAKCET => "",
            token::RIGHT_ANGLE_BRACKET => "",
            token::DASH => "",
            token::BACKTICK => "",
            token::LEFT_BRACKET => "",
            token::RIGHT_BRACKET => "",
            token::LEFT_PARENS => "",
            token::RIGHT_PARENS => "",
            token::EXCLAMATION_MARK => "",
            token::PLUS_SIGN => "",
            token::DOT => "",
            token::PIPE => "",
            token::BACKSLASH => "",
        };
    }

    fn new_token(token_type: token::Token, ch: u8) {

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
    #[test]
    fn test_next_token() {
        let test_string = String::from("this is a test.");
        let mut my_lexer = Lexer::new(test_string);
        my_lexer.read_char();
        assert_eq!(my_lexer.ch, 't');
    }
}
