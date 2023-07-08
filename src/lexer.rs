use crate::token::{Token, TokenType};

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

    pub fn next_token(&mut self) -> Option<Token> {
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
            '\0' => return None,
            oth => {
                if oth.is_letter() {
                    let literal = self.read_identifier();
                    return Some(Token::new(Token::lookup_ident(&literal), literal));
                } else if oth.is_ascii_digit() {
                    return Some(Token::new(TokenType::INT, self.read_number()));
                } else {
                    Token::new(TokenType::ILLEGAL, self.ch.to_string())
                }
            }
        };
        self.next_char();
        Some(t)
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
            Some(Token::new(TokenType::LET, "let".to_string())),
            Some(Token::new(TokenType::IDENT, "five".to_string())),
            Some(Token::new(TokenType::ASSIGN, "=".to_string())),
            Some(Token::new(TokenType::INT, "5".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ';'.to_string())),
            //
            Some(Token::new(TokenType::LET, "let".to_string())),
            Some(Token::new(TokenType::IDENT, "ten".to_string())),
            Some(Token::new(TokenType::ASSIGN, "=".to_string())),
            Some(Token::new(TokenType::INT, "10".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ';'.to_string())),
            //
            Some(Token::new(TokenType::LET, "let".to_string())),
            Some(Token::new(TokenType::IDENT, "add".to_string())),
            Some(Token::new(TokenType::ASSIGN, "=".to_string())),
            Some(Token::new(TokenType::FUNCTION, "fn".to_string())),
            Some(Token::new(TokenType::LPAREN, "(".to_string())),
            Some(Token::new(TokenType::IDENT, "x".to_string())),
            Some(Token::new(TokenType::COMMA, ",".to_string())),
            Some(Token::new(TokenType::IDENT, "y".to_string())),
            Some(Token::new(TokenType::RPAREN, ")".to_string())),
            //
            Some(Token::new(TokenType::LCURL, "{".to_string())),
            Some(Token::new(TokenType::IDENT, "x".to_string())),
            Some(Token::new(TokenType::PLUS, "+".to_string())),
            Some(Token::new(TokenType::IDENT, "y".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ';'.to_string())),
            Some(Token::new(TokenType::RCURL, "}".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ';'.to_string())),
            //
            Some(Token::new(TokenType::LET, "let".to_string())),
            Some(Token::new(TokenType::IDENT, "result".to_string())),
            Some(Token::new(TokenType::ASSIGN, "=".to_string())),
            Some(Token::new(TokenType::IDENT, "add".to_string())),
            Some(Token::new(TokenType::LPAREN, "(".to_string())),
            Some(Token::new(TokenType::IDENT, "five".to_string())),
            Some(Token::new(TokenType::COMMA, ",".to_string())),
            Some(Token::new(TokenType::IDENT, "ten".to_string())),
            Some(Token::new(TokenType::RPAREN, ")".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ';'.to_string())),
            //
            Some(Token::new(TokenType::BANG, '!'.to_string())),
            Some(Token::new(TokenType::MINUS, '-'.to_string())),
            Some(Token::new(TokenType::SLASH, '/'.to_string())),
            Some(Token::new(TokenType::ASTRIX, '*'.to_string())),
            Some(Token::new(TokenType::INT, '5'.to_string())),
            Some(Token::new(TokenType::SEMICOLON, ';'.to_string())),
            //
            Some(Token::new(TokenType::INT, '5'.to_string())),
            Some(Token::new(TokenType::LT, '<'.to_string())),
            Some(Token::new(TokenType::INT, "10".to_string())),
            Some(Token::new(TokenType::GT, ">".to_string())),
            Some(Token::new(TokenType::INT, "5".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ";".to_string())),
            //
            Some(Token::new(TokenType::IF, "if".to_string())),
            Some(Token::new(TokenType::LPAREN, "(".to_string())),
            Some(Token::new(TokenType::INT, "5".to_string())),
            Some(Token::new(TokenType::GT, ">".to_string())),
            Some(Token::new(TokenType::INT, "10".to_string())),
            Some(Token::new(TokenType::RPAREN, ")".to_string())),
            Some(Token::new(TokenType::LCURL, "{".to_string())),
            //
            Some(Token::new(TokenType::RETURN, "return".to_string())),
            Some(Token::new(TokenType::FALSE, "false".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ";".to_string())),
            //
            Some(Token::new(TokenType::RCURL, '}'.to_string())),
            Some(Token::new(TokenType::ELSE, "else".to_string())),
            Some(Token::new(TokenType::LCURL, "{".to_string())),
            //
            Some(Token::new(TokenType::RETURN, "return".to_string())),
            Some(Token::new(TokenType::TRUE, "true".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ";".to_string())),
            //
            Some(Token::new(TokenType::RCURL, '}'.to_string())),
            //
            Some(Token::new(TokenType::INT, "10".to_string())),
            Some(Token::new(TokenType::NEQ, "!=".to_string())),
            Some(Token::new(TokenType::INT, "9".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ";".to_string())),
            //
            Some(Token::new(TokenType::INT, "10".to_string())),
            Some(Token::new(TokenType::EQ, "==".to_string())),
            Some(Token::new(TokenType::INT, "10".to_string())),
            Some(Token::new(TokenType::SEMICOLON, ";".to_string())),
            //
            None,
        ];

        let mut lex = super::Lexer::new(input.to_string());

        for expected in expected_tokens.iter() {
            let tok = lex.next_token();
            if &tok != expected {
                println!("Expected: {:?}", expected);
                println!("Got: {:?}\n", tok);
            }
        }
    }
}
