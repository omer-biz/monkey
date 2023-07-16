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
        (&'a'..=&'z').contains(&self) || (&'A'..=&'Z').contains(&self) || self == &'_'
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.to_string(),
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
                    return Some(Token::new(TokenType::INT, &self.read_number()));
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

        self.input
            .chars()
            .skip(old_pos)
            .take(self.position - old_pos)
            .collect()
    }

    fn read_number(&mut self) -> String {
        let old_pos = self.position;

        while self.ch.is_ascii_digit() {
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
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

mod tests {

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
"#;

        let expected_tokens = vec![
            Some(Token::new(TokenType::LET, "let")),
            Some(Token::new(TokenType::IDENT, "five")),
            Some(Token::new(TokenType::ASSIGN, "=")),
            Some(Token::new(TokenType::INT, "5")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::LET, "let")),
            Some(Token::new(TokenType::IDENT, "ten")),
            Some(Token::new(TokenType::ASSIGN, "=")),
            Some(Token::new(TokenType::INT, "10")),
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
            Some(Token::new(TokenType::INT, "5")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::INT, "5")),
            Some(Token::new(TokenType::LT, "<")),
            Some(Token::new(TokenType::INT, "10")),
            Some(Token::new(TokenType::GT, ">")),
            Some(Token::new(TokenType::INT, "5")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::IF, "if")),
            Some(Token::new(TokenType::LPAREN, "(")),
            Some(Token::new(TokenType::INT, "5")),
            Some(Token::new(TokenType::GT, ">")),
            Some(Token::new(TokenType::INT, "10")),
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
            Some(Token::new(TokenType::INT, "10")),
            Some(Token::new(TokenType::NEQ, "!=")),
            Some(Token::new(TokenType::INT, "9")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            Some(Token::new(TokenType::INT, "10")),
            Some(Token::new(TokenType::EQ, "==")),
            Some(Token::new(TokenType::INT, "10")),
            Some(Token::new(TokenType::SEMICOLON, ";")),
            //
            None,
        ];

        let mut lex = super::Lexer::new(input);

        for expected in expected_tokens.iter() {
            let tok = lex.next_token();
            if &tok != expected {
                println!("Expected: {:?}", expected);
                println!("Got: {:?}\n", tok);
            }
        }
    }
}
