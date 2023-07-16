use lazy_static::lazy_static;

use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut keywords = HashMap::new();
        keywords.insert("let", TokenType::LET);
        keywords.insert("fn", TokenType::FUNCTION);
        keywords.insert("return", TokenType::RETURN);
        keywords.insert("false", TokenType::FALSE);
        keywords.insert("true", TokenType::TRUE);
        keywords.insert("else", TokenType::ELSE);
        keywords.insert("if", TokenType::IF);

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

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(ttype: TokenType, literal: &str) -> Self {
        Self {
            ttype,
            literal: literal.to_string(),
        }
    }

    pub fn lookup_ident(ident: &str) -> TokenType {
        KEYWORDS.get(ident).unwrap_or(&TokenType::IDENT).clone()
    }
}
