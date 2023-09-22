use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    line: usize,
    colm: usize,
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        (&'a'..=&'z').contains(&self) || (&'A'..=&'Z').contains(&self) || self == &'_'
    }
}

trait IsNewLine {
    fn is_newline(&self) -> bool;
}

impl IsNewLine for char {
    fn is_newline(&self) -> bool {
        self == &'\n'
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\0',
            line: 1,
            colm: 1,
        };
        l.next_char();
        l
    }

    pub fn next_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.read_position)
                .expect("bounded checked");
            self.colm += 1;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let t = match self.ch {
            '!' => {
                if self.peek_char() == '=' {
                    self.next_char();
                    Token::new(TokenType::NEQ, "!=".to_string().as_str())
                } else {
                    Token::new(TokenType::BANG, self.ch.to_string().as_str())
                }
            }
            '=' => {
                //
                if self.peek_char() == '=' {
                    self.next_char();
                    Token::new(TokenType::EQ, "==".to_string().as_str())
                } else {
                    Token::new(TokenType::ASSIGN, self.ch.to_string().as_str())
                }
            }
            ',' => Token::new(TokenType::COMMA, self.ch.to_string().as_str()),
            ';' => Token::new(TokenType::SEMICOLON, self.ch.to_string().as_str()),
            '(' => Token::new(TokenType::LPAREN, self.ch.to_string().as_str()),
            ')' => Token::new(TokenType::RPAREN, self.ch.to_string().as_str()),
            '{' => Token::new(TokenType::LCURL, self.ch.to_string().as_str()),
            '}' => Token::new(TokenType::RCURL, self.ch.to_string().as_str()),
            '+' => Token::new(TokenType::PLUS, self.ch.to_string().as_str()),
            '<' => Token::new(TokenType::LT, self.ch.to_string().as_str()),
            '>' => Token::new(TokenType::GT, self.ch.to_string().as_str()),
            '-' => Token::new(TokenType::MINUS, self.ch.to_string().as_str()),
            '*' => Token::new(TokenType::ASTRIX, self.ch.to_string().as_str()),
            '/' => Token::new(TokenType::SLASH, self.ch.to_string().as_str()),
            '\0' => return None,
            oth => {
                if oth.is_letter() {
                    let literal = self.read_identifier();
                    return Some(Token::new(Token::lookup_ident(&literal), &literal));
                } else if oth.is_ascii_digit() {
                    return Some(Token::new(TokenType::NUM, &self.read_number()));
                } else {
                    Token::new(TokenType::ILLEGAL, &self.ch.to_string())
                }
            }
        };
        self.next_char();
        Some(t)
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            if self.ch.is_newline() {
                self.line += 1;
                self.colm = 1;
            }

            self.next_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let old_pos = self.position;

        while self.ch.is_letter() {
            self.next_char();
        }

        self.input
            .chars()
            .skip(old_pos)
            .take(self.position - old_pos)
            .collect()
    }

    fn read_number(&mut self) -> String {
        let old_pos = self.position;

        while self.ch.is_ascii_digit() || self.ch == '.' {
            self.next_char();
        }

        self.input
            .chars()
            .skip(old_pos)
            .take(self.position - old_pos)
            .collect()
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input
                .chars()
                .nth(self.read_position)
                .expect("peek_char_fail")
        }
    }
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.colm)
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

mod tests {

    #[test]
    fn test_line_number() {
        use super::Lexer;
        use crate::token::{Token, TokenType};

        let input = r#"let one = 1;
let two = 2;
let three = 3;
let four = 4;"#;

        let expected = [
            ((1, 5), Token::new(TokenType::LET, "let")),
            ((1, 9), Token::new(TokenType::IDENT, "one")),
            ((1, 11), Token::new(TokenType::ASSIGN, "=")),
            ((1, 13), Token::new(TokenType::NUM, "1")),
            ((1, 14), Token::new(TokenType::SEMICOLON, ";")),
            ((2, 5), Token::new(TokenType::LET, "let")),
            ((2, 9), Token::new(TokenType::IDENT, "two")),
            ((2, 11), Token::new(TokenType::ASSIGN, "=")),
            ((2, 13), Token::new(TokenType::NUM, "2")),
            ((2, 14), Token::new(TokenType::SEMICOLON, ";")),
            ((3, 5), Token::new(TokenType::LET, "let")),
            ((3, 11), Token::new(TokenType::IDENT, "three")),
            ((3, 13), Token::new(TokenType::ASSIGN, "=")),
            ((3, 15), Token::new(TokenType::NUM, "3")),
            ((3, 16), Token::new(TokenType::SEMICOLON, ";")),
            ((4, 5), Token::new(TokenType::LET, "let")),
            ((4, 10), Token::new(TokenType::IDENT, "four")),
            ((4, 12), Token::new(TokenType::ASSIGN, "=")),
            ((4, 14), Token::new(TokenType::NUM, "4")),
            ((4, 14), Token::new(TokenType::SEMICOLON, ";")),
        ];

        let mut lexer = Lexer::new(input);

        for (exp_pos, exp_tok) in expected {
            let tok = lexer.next().expect("No token found");
            assert_eq!(exp_pos, lexer.position(), "The token positions don't match");
            assert_eq!(exp_tok, tok, "The token's don't match");
        }
    }

    #[test]
    fn test_next_token() {
        use crate::token::{Token, TokenType};

        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);

!-/*5;

5 < 10 > 5;

if (5 > 10) {
    return false;
} else {
    return true;
}

10 != 9;
10 == 10;
1.101 != 2.202;
"#;

        let expected_tokens = vec![
            Some(Token::new(TokenType::LET, "let")),
            Some(Token::new(TokenType::IDENT, "five")),
            Some(Token::new(TokenType::ASSIGN, "=")),
            Some(Token::new(TokenType::NUM, "5")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::LET, "let")),
            Some(Token::new(TokenType::IDENT, "ten")),
            Some(Token::new(TokenType::ASSIGN, "=")),
            Some(Token::new(TokenType::NUM, "10")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::LET, "let")),
            Some(Token::new(TokenType::IDENT, "add")),
            Some(Token::new(TokenType::ASSIGN, "=")),
            Some(Token::new(TokenType::FUNCTION, "fn")),
            Some(Token::new(TokenType::LPAREN, "(")),
            Some(Token::new(TokenType::IDENT, "x")),
            Some(Token::new(TokenType::COMMA, ",")),
            Some(Token::new(TokenType::IDENT, "y")),
            Some(Token::new(TokenType::RPAREN, ")")),
            //
            Some(Token::new(TokenType::LCURL, "{")),
            Some(Token::new(TokenType::IDENT, "x")),
            Some(Token::new(TokenType::PLUS, "+")),
            Some(Token::new(TokenType::IDENT, "y")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            Some(Token::new(TokenType::RCURL, "}")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::LET, "let")),
            Some(Token::new(TokenType::IDENT, "result")),
            Some(Token::new(TokenType::ASSIGN, "=")),
            Some(Token::new(TokenType::IDENT, "add")),
            Some(Token::new(TokenType::LPAREN, "(")),
            Some(Token::new(TokenType::IDENT, "five")),
            Some(Token::new(TokenType::COMMA, ",")),
            Some(Token::new(TokenType::IDENT, "ten")),
            Some(Token::new(TokenType::RPAREN, ")")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::BANG, "!")),
            Some(Token::new(TokenType::MINUS, "-")),
            Some(Token::new(TokenType::SLASH, "/")),
            Some(Token::new(TokenType::ASTRIX, "*")),
            Some(Token::new(TokenType::NUM, "5")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::NUM, "5")),
            Some(Token::new(TokenType::LT, "<")),
            Some(Token::new(TokenType::NUM, "10")),
            Some(Token::new(TokenType::GT, ">")),
            Some(Token::new(TokenType::NUM, "5")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::IF, "if")),
            Some(Token::new(TokenType::LPAREN, "(")),
            Some(Token::new(TokenType::NUM, "5")),
            Some(Token::new(TokenType::GT, ">")),
            Some(Token::new(TokenType::NUM, "10")),
            Some(Token::new(TokenType::RPAREN, ")")),
            Some(Token::new(TokenType::LCURL, "{")),
            //
            Some(Token::new(TokenType::RETURN, "return")),
            Some(Token::new(TokenType::FALSE, "false")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::RCURL, "}")),
            Some(Token::new(TokenType::ELSE, "else")),
            Some(Token::new(TokenType::LCURL, "{")),
            //
            Some(Token::new(TokenType::RETURN, "return")),
            Some(Token::new(TokenType::TRUE, "true")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::RCURL, "}")),
            //
            Some(Token::new(TokenType::NUM, "10")),
            Some(Token::new(TokenType::NEQ, "!=")),
            Some(Token::new(TokenType::NUM, "9")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::NUM, "10")),
            Some(Token::new(TokenType::EQ, "==")),
            Some(Token::new(TokenType::NUM, "10")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::NUM, "1.101")),
            Some(Token::new(TokenType::NEQ, "!=")),
            Some(Token::new(TokenType::NUM, "2.202")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            None,
        ];

        let mut lex = super::Lexer::new(input);

        for expected in expected_tokens.iter() {
            let tok = lex.next_token();
            assert_eq!(&tok, expected, "Expected: {:?} Got: {:?}\n", expected, tok);
        }
    }
}
