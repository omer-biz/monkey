use crate::ast::{Identifier, LetStatement, Program, Statements};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            cur_token: None,
            peek_token: None,
            lexer,
        };
        p.next_token();
        p.next_token();
        p
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    pub fn cur_token_is(&mut self, token_type: &TokenType) -> bool {
        if let Some(token) = &self.cur_token {
            return &token.ttype == token_type;
        } else {
            return false;
        }
    }

    pub fn peek_token(&mut self, tok: &Token) -> bool {
        if let Some(peek_tok) = &self.peek_token {
            if peek_tok == tok {
                return true;
            }
        }
        return false;
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.is_some() {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }

            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statements> {
        match self.cur_token {
            Some(Token {
                ttype: TokenType::LET,
                literal: _,
            }) => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statements> {
        let let_token = Token {
            ttype: TokenType::LET,
            literal: self.cur_token.take().expect("resolved").literal,
        };

        if !self.expect_peek_is(TokenType::IDENT) {
            return None;
        }

        let ident_tokn = self.cur_token.clone().expect("checked");
        let name = Identifier {
            token: ident_tokn.clone(),
            value: ident_tokn.literal,
        };

        if !self.expect_peek_is(TokenType::ASSIGN) {
            return None;
        }

        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statements::from(LetStatement {
            name,
            token: let_token,
            value: None,
        }))
    }

    fn expect_peek_is(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            return &token.ttype == token_type;
        } else {
            return false;
        }
    }
}

mod tests {
    use crate::ast::{LetStatement, Node, Statement, Statements};

    #[test]
    fn test_let_statements() {
        use super::Parser;
        use crate::lexer::Lexer;

        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = barfoo;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.statements.len() != 3 {
            panic!("program.Statments does not contain 3 statements")
        }

        let exprected_ident = vec!["x", "y", "foobar"];

        for (i, ident) in exprected_ident.iter().enumerate() {
            let stmt = program
                .statements
                .get(i)
                .expect("statment not found at index");

            test_let_statement(stmt, ident);
        }
    }

    fn test_let_statement(stmt: &Statements, ident: &str) {
        if stmt.token_literal() != "let" {
            panic!("stmt.token_literal not 'let'. got={}", stmt.token_literal())
        }

        let let_stmt = match stmt {
            Statements::LetStmt(let_stmt) => let_stmt,
        };

        if let_stmt.name.value != ident {
            panic!(
                "let_stmt.name.value not {} got {}",
                ident, let_stmt.name.value
            );
        }

        if let_stmt.name.token_literal() != ident {
            panic!(
                "let_stmt.name.token_literal() not {} got {}",
                ident,
                let_stmt.name.token_literal()
            )
        }
    }
}
