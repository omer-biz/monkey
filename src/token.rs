use lazy_static::lazy_static;

use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = {
        let mut keywords = HashMap::new();
        keywords.insert("let".to_string(), TokenType::LET);
        keywords.insert("fn".to_string(), TokenType::FUNCTION);
        keywords.insert("return".to_string(), TokenType::RETURN);
        keywords.insert("false".to_string(), TokenType::FALSE);
        keywords.insert("true".to_string(), TokenType::TRUE);
        keywords.insert("else".to_string(), TokenType::ELSE);
        keywords.insert("if".to_string(), TokenType::IF);

        keywords
    };
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    MINUS,
    SLASH,
    ASTRIX,
    NEQ,
    TRUE,
    ELSE,
    RETURN,
    FALSE,
    LET,
    FUNCTION,
    LT,
    GT,
    LPAREN,
    RPAREN,
    LCURL,
    RCURL,
    PLUS,
    IDENT,
    BANG,
    ASSIGN,
    COMMA,
    SEMICOLON,
    ILLEGAL,
    EOF,
    INT,

    IF,
    EQ,
}

#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(ttype: TokenType, literal: String) -> Self {
        Self { ttype, literal }
    }

    pub fn lookup_ident(ident: &str) -> TokenType {
        KEYWORDS.get(ident).unwrap_or(&TokenType::IDENT).clone()
    }
}
