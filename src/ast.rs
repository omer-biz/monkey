use enum_dispatch::enum_dispatch;

use crate::token::Token;

#[enum_dispatch]
pub trait Node {
    fn token_literal(&self) -> &str;
    fn to_string(&self) -> &str;
}

#[enum_dispatch]
pub trait Statement: Node {
    fn statement_node(&self);
}

#[enum_dispatch]
pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expressions>,
}

#[enum_dispatch(Expression, Node)]
pub enum Expressions {
    Ident(Identifier),
}

pub struct Program {
    pub statements: Vec<Statements>,
}

impl Program {
    pub fn to_string(&self) -> String {
        let mut buffer: String = String::new();

        for stmt in &self.statements {
            buffer.push_str(&stmt.to_string())
        }

        buffer
    }
}

#[enum_dispatch(Statement, Node)]
pub enum Statements {
    LetStmt(LetStatement),
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn to_string(&self) -> &str {
        todo!()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn to_string(&self) -> &str {
        todo!()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
