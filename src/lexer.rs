use crate::token::{self, Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        return &'a' <= self && self <= &'z' || &'A' <= self && self <= &'Z' || self == &'_';
    }
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
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
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let t = match self.ch {
            '!' => {
                if self.peek_char() == '=' {
                    self.next_char();
                    Token::new(TokenType::NEQ, "!=".to_string())
                } else {
                    Token::new(TokenType::BANG, self.ch.to_string())
                }
            }
            '=' => {
                //
                if self.peek_char() == '=' {
                    self.next_char();
                    Token::new(TokenType::EQ, "==".to_string())
                } else {
                    Token::new(TokenType::ASSIGN, self.ch.to_string())
                }
            }
            ',' => Token::new(TokenType::COMMA, self.ch.to_string()),
            ';' => Token::new(TokenType::SEMICOLON, self.ch.to_string()),
            '(' => Token::new(TokenType::LPAREN, self.ch.to_string()),
            ')' => Token::new(TokenType::RPAREN, self.ch.to_string()),
            '{' => Token::new(TokenType::LCURL, self.ch.to_string()),
            '}' => Token::new(TokenType::RCURL, self.ch.to_string()),
            '+' => Token::new(TokenType::PLUS, self.ch.to_string()),
            '<' => Token::new(TokenType::LT, self.ch.to_string()),
            '>' => Token::new(TokenType::GT, self.ch.to_string()),
            '-' => Token::new(TokenType::MINUS, self.ch.to_string()),
            '*' => Token::new(TokenType::ASTRIX, self.ch.to_string()),
            '/' => Token::new(TokenType::SLASH, self.ch.to_string()),
            '\0' => Token::new(TokenType::EOF, self.ch.to_string()),
            oth => {
                if oth.is_letter() {
                    let literal = self.read_identifier();
                    return Token::new(Token::lookup_ident(&literal), literal);
                } else if oth.is_ascii_digit() {
                    return Token::new(TokenType::INT, self.read_number());
                } else {
                    Token::new(TokenType::ILLEGAL, self.ch.to_string())
                }
            }
        };
        self.next_char();
        t
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            // println!("cur char: '{}'", self.ch);
            // println!(
            //     "nex char: '{:?}'",
            //     self.input.chars().nth(self.read_position)
            // );

            self.next_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let old_pos = self.position;

        while self.ch.is_letter() {
            self.next_char();
        }

        let ret = self
            .input
            .chars()
            .skip(old_pos)
            .take(self.position - old_pos)
            .collect();

        ret
    }

    fn read_number(&mut self) -> String {
        let old_pos = self.position;

        while self.ch.is_ascii_digit() {
            self.next_char();
        }

        let ret = self
            .input
            .chars()
            .skip(old_pos)
            .take(self.position - old_pos)
            .collect();

        ret
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        } else {
            self.input
                .chars()
                .nth(self.read_position)
                .expect("peek_char_fail")
        }
    }
}

mod tests {
    use crate::token::{Token, TokenType};

    use super::Lexer;

    #[derive(Debug)]
    struct ExpectedToken {
        ttype: TokenType,
        literal: String,
    }

    #[test]
    fn test_next_token() {
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
"#;

        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ';'.to_string()),
            //
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ';'.to_string()),
            //
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            //
            Token::new(TokenType::LCURL, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ';'.to_string()),
            Token::new(TokenType::RCURL, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ';'.to_string()),
            //
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ';'.to_string()),
            //
            Token::new(TokenType::BANG, '!'.to_string()),
            Token::new(TokenType::MINUS, '-'.to_string()),
            Token::new(TokenType::SLASH, '/'.to_string()),
            Token::new(TokenType::ASTRIX, '*'.to_string()),
            Token::new(TokenType::INT, '5'.to_string()),
            Token::new(TokenType::SEMICOLON, ';'.to_string()),
            //
            Token::new(TokenType::INT, '5'.to_string()),
            Token::new(TokenType::LT, '<'.to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            //
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LCURL, "{".to_string()),
            //
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            //
            Token::new(TokenType::RCURL, '}'.to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::LCURL, "{".to_string()),
            //
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            //
            Token::new(TokenType::RCURL, '}'.to_string()),
            //
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::NEQ, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            //
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            //
            Token::new(TokenType::EOF, '\0'.to_string()),
        ];

        let mut lex = Lexer::new(input.to_string());

        // for (i, expected) in expected_tokens.iter().enumerate() {
        for expected in expected_tokens.iter() {
            let tok = lex.next_token();

            println!("Expected: {:?}", expected);
            println!("Got: {:?}\n", tok);

            if tok.ttype != expected.ttype {
                panic!("unexpedted type")
            }

            if tok.literal != expected.literal {
                panic!("unexpedted literal")
            }
        }
    }
}
