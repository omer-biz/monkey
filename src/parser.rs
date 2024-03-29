use lazy_static::lazy_static;
use std::{collections::HashMap, vec};

use crate::{
    ast::{
        BlockStatement, Boolean, CallExpressions, ExpressionStatement, Expressions,
        FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statements,
    },
    error::Result,
    missing_token,
    lexer::Lexer,
    token::{Token, TokenType}, expected_token, no_parsing_function,
};

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut precedences = HashMap::new();
        precedences.insert(TokenType::LPAREN, Precedence::Call);
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

type PrefixParseFn = for<'a> fn(&'a mut Parser) -> Result<Expressions>;
type InfixParseFn = for<'a> fn(&'a mut Parser, Expressions) -> Result<Expressions>;

#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    Lessgreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,

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
        p.register_infix(TokenType::LPAREN, Parser::parse_call_expression);

        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_call_expression(&mut self, function: Expressions) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        let arguments = self.parse_call_arguments();

        Ok(Expressions::from(CallExpressions {
            token,
            arguments,
            function: Box::new(function),
        }))
    }

    pub fn parse_function_literal(&mut self) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        if !self.expect_peek_is(TokenType::LPAREN) {
            panic!()
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek_is(TokenType::LCURL) {
            panic!()
        }

        let body = self.parse_block_statement()?;

        Ok(Expressions::from(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    pub fn parse_if_expression(&mut self) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

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

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek_is(TokenType::LCURL) {
                None
            } else {
                Some(self.parse_block_statement()?)
            }
        } else {
            None
        };

        Ok(Expressions::from(IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
            token,
        }))
    }

    pub fn parse_grouped_expression(&mut self) -> Result<Expressions> {
        self.next_token();
        let exp = self
            .parse_expression(&Precedence::Lowest)?;
            

        if !self.expect_peek_is(TokenType::RPAREN) {
            println!("exp: {exp:#?}");
            panic!("No left paren found at: {:?}", self.lexer.position())
        }

        Ok(exp)
    }

    pub fn parse_boolean(&mut self) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        let value = token.literal.parse::<bool>().unwrap_or(false);

        Ok(Expressions::from(Boolean { value, token }))
    }

    pub fn parse_infix_expression(&mut self, left: Expressions) -> Result<Expressions> {
        let precedence = self.cur_precedence();
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        let operator = token.literal.to_string();

        self.next_token();

        let right = self
            .parse_expression(precedence)
            .expect("no right expression found");

        Ok(Expressions::from(InfixExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator,
            token,
        }))
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

    pub fn parse_prefix_expression(&mut self) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        let operator = token.literal.chars().next().expect("operator not found");
        self.next_token();
        let right = Box::new(
            self.parse_expression(&Precedence::Prefix)
                .expect("no expression found"),
        );

        Ok(Expressions::from(PrefixExpression {
            operator,
            right,
            token,
        }))
    }

    pub fn no_prefix_parse_fn_error(&mut self) {
        let msg = format!("no prefix parse function for {:?} found", self.cur_token);
        self.errors.push(msg);
    }

    pub fn parse_identifer(&mut self) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        Ok(Expressions::from(Identifier {
            value: token.literal.clone(),
            token,
        }))
    }

    pub fn parse_integers(&mut self) -> Result<Expressions> {
        let token = self
            .cur_token
            .take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        Ok(Expressions::Integ(IntegerLiteral {
            value: token.literal.parse().expect("can't convert literal"),
            token,
        }))
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
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => println!("Error: {:?}", err)
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statements> {
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

    fn parse_let_statement(&mut self) -> Result<Statements> {
        let let_token = self.cur_token.take().ok_or_else(missing_token!(self.lexer.position()))?;

        if !self.expect_peek_is(TokenType::IDENT) {
            return Err(expected_token!(self.lexer.position(), TokenType::IDENT));
        }

        let ident_tokn = self.cur_token.take().ok_or_else(missing_token!(self.lexer.position()))?;
        let name = Identifier {
            token: ident_tokn.clone(),
            value: ident_tokn.literal,
        };

        if !self.expect_peek_is(TokenType::ASSIGN) {
            return Err(expected_token!(self.lexer.position(), TokenType::ASSIGN));
        }

        self.next_token();

        let value = self
            .parse_expression(&Precedence::Lowest)
            .expect("can't find expression");

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statements::from(LetStatement {
            name,
            token: let_token,
            value,
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

    fn parse_return_statement(&mut self) -> Result<Statements> {
        let ret_token = self.cur_token.take().ok_or_else(missing_token!(self.lexer.position()))?;


        self.next_token();

        let return_value = self
            .parse_expression(&Precedence::Lowest)
            .expect("can't find an expression");

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statements::from(ReturnStatement {
            token: ret_token,
            return_value,
        }))
    }

    fn register_prefix(&mut self, token_type: TokenType, prefix_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, prefix_fn);
    }

    fn register_infix(&mut self, token_type: TokenType, infix_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, infix_fn);
    }

    fn parse_expression_statement(&mut self) -> Result<Statements> {
        let token = self
            .cur_token
            .clone()
            .ok_or_else(missing_token!(self.lexer.position()))?;


        let expression = self.parse_expression(&Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statements::from(ExpressionStatement { token, expression }))
    }

    fn parse_expression(&mut self, precedence: &Precedence) -> Result<Expressions> {
        let Some(&prefix) = self.prefix_parse_fns.get(
            self.cur_token
                .as_ref()
                .map(|t| &t.ttype)
                .ok_or_else(missing_token!(self.lexer.position()))?
                
        ) else {
            self.no_prefix_parse_fn_error();
            return Err(no_parsing_function!(self.lexer.position(), TokenType::ASSIGN));
        };

        let mut left_expression = prefix(self);

        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let Some(&infix) = self.infix_parse_fns.get(
                self.peek_token
                    .as_ref()
                    .map(|t| &t.ttype)
                    .ok_or_else(missing_token!(self.lexer.position()))?,
            ) else {
                return left_expression;
            };

            self.next_token();
            left_expression = infix(self, left_expression?);
        }

        left_expression
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let token = self.cur_token.take()
            .ok_or_else(missing_token!(self.lexer.position()))?;

        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(&TokenType::RCURL) && !self.cur_token_is(&TokenType::EOF) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt)
            }
            self.next_token();
        }

        Ok(BlockStatement { token, statements })
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

    fn parse_call_arguments(&mut self) -> Vec<Expressions> {
        let mut args = vec![];

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(
            self.parse_expression(&Precedence::Lowest)
                .expect("no expression found"),
        );

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();

            args.push(
                self.parse_expression(&Precedence::Lowest)
                    .expect("no expression found"),
            );
        }

        if !self.expect_peek_is(TokenType::RPAREN) {
            panic!("no closing parenthesis");
        }

        args
    }
}

mod tests;
