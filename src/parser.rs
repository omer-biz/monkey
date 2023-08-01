use lazy_static::lazy_static;
use std::collections::HashMap;

use crate::{
    ast::{
        BlockStatement, Boolean, ExpressionStatement, Expressions, FunctionLiteral, Identifier,
        IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program,
        ReturnStatement, Statements,
    },
    lexer::Lexer,
    token::{Token, TokenType},
};

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut precedences = HashMap::new();
        precedences.insert(TokenType::EQ, Precedence::Equals);
        precedences.insert(TokenType::NEQ, Precedence::Equals);
        precedences.insert(TokenType::LT, Precedence::Lessgreater);
        precedences.insert(TokenType::GT, Precedence::Lessgreater);
        precedences.insert(TokenType::PLUS, Precedence::Sum);
        precedences.insert(TokenType::MINUS, Precedence::Sum);
        precedences.insert(TokenType::SLASH, Precedence::Product);
        precedences.insert(TokenType::ASTRIX, Precedence::Product);

        precedences
    };
}

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Expressions;
type InfixParseFn = for<'a> fn(&'a mut Parser, Expressions) -> Expressions;

#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    Lessgreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
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

        p.register_prefix(TokenType::IDENT, Parser::parse_identifer);
        p.register_prefix(TokenType::NUM, Parser::parse_integers);
        p.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);

        p.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        p.register_prefix(TokenType::FALSE, Parser::parse_boolean);

        p.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);

        p.register_prefix(TokenType::IF, Parser::parse_if_expression);
        p.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);

        p.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        p.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        p.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        p.register_infix(TokenType::ASTRIX, Parser::parse_infix_expression);
        p.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        p.register_infix(TokenType::NEQ, Parser::parse_infix_expression);
        p.register_infix(TokenType::LT, Parser::parse_infix_expression);
        p.register_infix(TokenType::GT, Parser::parse_infix_expression);

        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_function_literal(&mut self) -> Expressions {
        let token = self.cur_token.take().expect("token not found");
        if !self.expect_peek_is(TokenType::LPAREN) {
            panic!()
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek_is(TokenType::LCURL) {
            panic!()
        }

        let body = self.parse_block_statement();

        Expressions::from(FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    pub fn parse_if_expression(&mut self) -> Expressions {
        let token = self.cur_token.take().expect("token not found");

        if !self.expect_peek_is(TokenType::LPAREN) {
            panic!();
        }
        self.next_token();

        let condition = self
            .parse_expression(&Precedence::Lowest)
            .expect("no expressions found");

        if !self.expect_peek_is(TokenType::RPAREN) {
            panic!()
        }

        if !self.expect_peek_is(TokenType::LCURL) {
            panic!()
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek_is(TokenType::LCURL) {
                None
            } else {
                Some(self.parse_block_statement())
            }
        } else {
            None
        };

        Expressions::from(IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
            token,
        })
    }

    pub fn parse_grouped_expression(&mut self) -> Expressions {
        self.next_token();
        let exp = self
            .parse_expression(&Precedence::Lowest)
            .expect("could not parse input");

        if !self.expect_peek_is(TokenType::RPAREN) {
            println!("exp: {exp:#?}");
            panic!("no left paren found")
        }

        return exp;
    }

    pub fn parse_boolean(&mut self) -> Expressions {
        let token = self.cur_token.take().expect("no token found");
        let value = token.literal.parse::<bool>().unwrap_or(false);

        Expressions::from(Boolean { value, token })
    }

    pub fn parse_infix_expression(&mut self, left: Expressions) -> Expressions {
        let precedence = self.cur_precedence();
        let token = self.cur_token.take().expect("no token found");
        let operator = token.literal.to_string();

        self.next_token();

        let right = self
            .parse_expression(precedence)
            .expect("no right expression found");

        Expressions::from(InfixExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator,
            token,
        })
    }

    fn token_precedence<'precedence>(tt: Option<&Token>) -> &'precedence Precedence {
        PRECEDENCES
            .get(tt.as_ref().map(|t| &t.ttype).unwrap_or(&TokenType::EOF))
            .unwrap_or(&Precedence::Lowest)
    }

    fn peek_precedence<'precedence>(&self) -> &'precedence Precedence {
        Parser::token_precedence(self.peek_token.as_ref())
    }

    fn cur_precedence<'precedence>(&self) -> &'precedence Precedence {
        Parser::token_precedence(self.cur_token.as_ref())
    }

    pub fn parse_prefix_expression(&mut self) -> Expressions {
        let token = self.cur_token.take().expect("no token found");

        let operator = token.literal.chars().next().expect("operator not found");
        self.next_token();
        let right = Box::new(
            self.parse_expression(&Precedence::Prefix)
                .expect("no expression found"),
        );

        Expressions::from(PrefixExpression {
            operator,
            right,
            token,
        })
    }

    pub fn no_prefix_parse_fn_error(&mut self) {
        let msg = format!("no prefix parse function for {:?} found", self.cur_token);
        self.errors.push(msg);
    }

    pub fn parse_identifer(&mut self) -> Expressions {
        let token = self.cur_token.take().expect("token not found");

        Expressions::from(Identifier {
            value: token.literal.clone(),
            token,
        })
    }

    pub fn parse_integers(&mut self) -> Expressions {
        let token = self.cur_token.take().expect("token not found");

        Expressions::Integ(IntegerLiteral {
            value: token.literal.parse().expect("can't convert literal"),
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

        let expression = self.parse_expression(&Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statements::from(ExpressionStatement { token, expression }))
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Option<Expressions> {
        let Some(&prefix) = self.prefix_parse_fns.get(
            self.cur_token
                .as_ref()
                .map(|t| &t.ttype)
                .expect("token not found")
        ) else {
            self.no_prefix_parse_fn_error();
            return None;
        };

        let mut left_expression = prefix(self);

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let Some(&infix) = self.infix_parse_fns.get(
                self.peek_token
                    .as_ref()
                    .map(|t| &t.ttype)
                    .expect("token not found"),
            ) else {
                return Some(left_expression);
            };

            self.next_token();
            left_expression = infix(self, left_expression);
        }

        Some(left_expression)
    }

    fn parse_block_statement(&mut self) -> crate::ast::BlockStatement {
        let token = self.cur_token.take().expect("no token found");
        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(&TokenType::RCURL) && !self.cur_token_is(&TokenType::EOF) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt)
            }
            self.next_token();
        }

        BlockStatement { token, statements }
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = vec![];

        if self.peek_token_is(&TokenType::RPAREN) {
            println!("cur tok: {:#?}", self.cur_token);
            println!("pek tok: {:#?}", self.peek_token);
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let token = self.cur_token.take().expect("no identifer found");
        let ident = Identifier {
            value: token.literal.clone(),
            token,
        };
        identifiers.push(ident);

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();

            let token = self.cur_token.take().expect("no identifer found");
            let ident = Identifier {
                value: token.literal.clone(),
                token,
            };

            identifiers.push(ident);
        }

        if !self.expect_peek_is(TokenType::RPAREN) {
            panic!("no right paren found");
        }

        identifiers
    }
}

mod tests;
