use crate::token::TokenType;

pub type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    MissingToken {
        line: usize,
        colm: usize,
    },
    ExpectedToken {
        token_type: TokenType,
        line: usize,
        colm: usize,
    },
    NoParsingFunction {
        token_type: TokenType,
        line: usize,
        colm: usize,
    },
}

impl ParserError {
    pub fn new_pos_missing_token(pos: (usize, usize)) -> Self {
        ParserError::MissingToken {
            line: pos.0,
            colm: pos.1,
        }
    }

    pub fn new_missing_token(line: usize, colm: usize) -> Self {
        ParserError::MissingToken { line, colm }
    }

    pub fn new_pos_expected_token(pos: (usize, usize), token_type: TokenType) -> Self {
        ParserError::ExpectedToken {
            line: pos.0,
            colm: pos.1,
            token_type,
        }
    }

    pub fn new_expected_token(line: usize, colm: usize, token_type: TokenType) -> Self {
        ParserError::ExpectedToken {
            line,
            colm,
            token_type,
        }
    }

    pub fn new_pos_no_parsing_function(pos: (usize, usize), token_type: TokenType) -> Self {
        ParserError::NoParsingFunction {
            line: pos.0,
            colm: pos.1,
            token_type,
        }
    }

    pub fn new_no_parsing_function(line: usize, colm: usize, token_type: TokenType) -> Self {
        ParserError::NoParsingFunction {
            line,
            colm,
            token_type,
        }
    }
}

#[macro_export]
macro_rules! missing_token {
    ($pos:expr) => {{
        || crate::error::ParserError::new_pos_missing_token($pos)
    }};
}

#[macro_export]
macro_rules! expected_token {
    ($pos:expr, $tok:expr) => {{
        crate::error::ParserError::new_pos_expected_token($pos, $tok)
    }};
}

#[macro_export]
macro_rules! no_parsing_function {
    ($pos:expr, $tok:expr) => {{
        crate::error::ParserError::new_pos_no_parsing_function($pos, $tok)
    }};
}

mod test {}
