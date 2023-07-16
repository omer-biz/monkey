use crate::{
    ast::{Identifier, LetStatement, Program, ReturnStatement, Statements},
    lexer::Lexer,
    token::{Token, TokenType},
};

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            cur_token: None,
            peek_token: None,
            lexer,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }

    pub fn peek_error(&mut self, expected: &TokenType) {
        let msg = format!(
            "expected next token to be {:?} got '{}' instead",
            expected,
            self.peek_token
                .as_ref()
                .map(|s| s.literal.as_str())
                .unwrap_or("None")
        );

        self.errors.push(msg);
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    pub fn cur_token_is(&mut self, token_type: &TokenType) -> bool {
        if let Some(token) = &self.cur_token {
            &token.ttype == token_type
        } else {
            false
        }
    }

    pub fn peek_token(&mut self, tok: &Token) -> bool {
        if let Some(peek_tok) = &self.peek_token {
            if peek_tok == tok {
                return true;
            }
        }
        false
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
            Some(Token {
                ttype: TokenType::RETURN,
                literal: _,
            }) => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statements> {
        let let_token = self.cur_token.take().expect("`let_token` not found");

        if !self.expect_peek_is(TokenType::IDENT) {
            return None;
        }

        let ident_tokn = self.cur_token.take().expect("checked");
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
            true
        } else {
            self.peek_error(&token);
            false
        }
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            &token.ttype == token_type
        } else {
            false
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statements> {
        let ret_token = self.cur_token.take().expect("`return` not found");

        self.next_token();

        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statements::from(ReturnStatement {
            token: ret_token,
            value: None,
        }))
    }
}

mod tests {
    use crate::ast::{Node, Statements};

    #[test]
    pub fn test_let_statements() {
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
        check_parser_errors(&parser);

        if program.statements.len() != 3 {
            panic!("program.statments does not contain 3 statements")
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

    #[allow(dead_code)]
    fn check_parser_errors(parser: &super::Parser) {
        if parser.errors.len() == 0 {
            return;
        }

        for e in parser.errors.iter() {
            println!("{}", e);
        }

        panic!("parser error");
    }

    #[allow(dead_code)]
    pub fn test_let_statement(stmt: &Statements, ident: &str) {
        if stmt.token_literal() != "let" {
            panic!("stmt.token_literal not 'let'. got={}", stmt.token_literal())
        }

        let Statements::LetStmt(let_stmt) = stmt else { panic!("Statment not `LetStatment`") };

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

    #[test]
    pub fn test_return_statements() {
        use super::Parser;
        use crate::lexer::Lexer;

        let input = r#"
        return 5;
        return x;
        return x + 1;
        return 10 + 1;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 4 {
            panic!("program.statments does not contain 4 statements")
        }

        for stmt in program.statements.iter() {
            let Statements::RetStmt(return_statement) = stmt else { panic!("not return statement got {:?}", stmt) };
            if return_statement.token_literal() != "return" {
                panic!(
                    "return_statement.token_literal() not `return` got {:?}",
                    return_statement.token_literal()
                )
            }
        }
    }
}
