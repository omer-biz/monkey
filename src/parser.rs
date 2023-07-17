use std::collections::HashMap;

use crate::{
    ast::{
        ExpressionStatement, Expressions, Identifier, LetStatement, Program, ReturnStatement,
        Statements,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = for<'a> fn(&'a Parser) -> Expressions;
type InfixParseFn = for<'a> fn(&'a Parser, Expressions) -> Expressions;

#[repr(u8)]
enum Precedence {
    Lowest,
    // Equals,      // ==
    // Lessgreater, // > or <
    // Sum,         // +
    // Product,     // *
    // Prefix,      // -X or !X
    // Call,        // myFunction(X)
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            cur_token: None,
            peek_token: None,
            lexer,
            errors: vec![],

            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.prefix_parse_fns
            .insert(TokenType::IDENT, Parser::parse_identifer);

        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_identifer(&self) -> Expressions {
        let token = self.cur_token.clone().expect("token not found");

        Expressions::from(Identifier {
            value: token.literal.clone(),
            token,
        })
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
            _ => self.parse_expression_statement(),
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
            value: Expressions::from(Identifier {
                token: Token {
                    ttype: TokenType::IDENT,
                    literal: "".to_string(),
                },
                value: "".to_string(),
            }),
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
            return_value: None,
        }))
    }

    fn register_prefix(&mut self, token_type: TokenType, prefix_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, prefix_fn);
    }

    fn register_infix(&mut self, token_type: TokenType, infix_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, infix_fn);
    }

    fn parse_expression_statement(&mut self) -> Option<Statements> {
        let token = self
            .cur_token
            .clone()
            .expect("no token found on expression statement");

        let Some(expression) = self.parse_expression(Precedence::Lowest) else { return None };

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statements::from(ExpressionStatement { token, expression }))
    }

    fn parse_expression(&self, _lowest: Precedence) -> Option<Expressions> {
        let Some(&prefix) = self.prefix_parse_fns.get(
            self
                .cur_token
                .as_ref()
                .map(|t| &t.ttype)
                .expect("token not found"),
        ) else { return None };
        let left_expression = prefix(self);

        Some(left_expression)
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
        if parser.errors.is_empty() {
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

    #[test]
    pub fn test_as_string() {
        use crate::{
            ast::{Expressions, Identifier, LetStatement, Program, Statements},
            token::{Token, TokenType},
        };

        let program = Program {
            statements: vec![Statements::from(LetStatement {
                token: Token {
                    ttype: TokenType::LET,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        ttype: TokenType::IDENT,
                        literal: "my_var".to_string(),
                    },
                    value: "my_var".to_string(),
                },
                value: Expressions::from(Identifier {
                    token: Token {
                        ttype: TokenType::IDENT,
                        literal: "another_var".to_string(),
                    },
                    value: "another_var".to_string(),
                }),
            })],
        };

        if program.to_string() != "let my_var = another_var;" {
            panic!("program.to_string() wrong. got={:?}", program.to_string());
        }
    }

    #[test]
    pub fn test_identifer_expression() {
        use super::Parser;
        use crate::{
            ast::{Expressions, Statements},
            lexer::Lexer,
        };

        let input = r#"
        foobar;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!()
        }

        for expression in program.statements.iter() {
            let Statements::ExpStmt(exp_stmt) = expression else { panic!() };

            let Expressions::Ident(ident) = &exp_stmt.expression;

            if ident.value != "foobar" {
                panic!();
            }

            if ident.token_literal() != "foobar" {
                panic!();
            }
        }
    }
}
