use std::fmt::Display;

use enum_dispatch::enum_dispatch;

use crate::token::Token;

#[enum_dispatch]
pub trait Node {
    fn token_literal(&self) -> &str;
    fn as_str(&self) -> &str;
}

#[enum_dispatch]
pub trait Statement: Node {
    fn statement_node(&self);
}

#[enum_dispatch]
pub trait Expression: Node {
    fn expression_node(&self);
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expressions>,
}

#[enum_dispatch(Expression, Node)]
#[derive(Debug)]
pub enum Expressions {
    Ident(Identifier),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statements>,
}

impl Program {
    pub fn tree(&self) -> String {
        let mut buffer: String = String::new();

        for stmt in &self.statements {
            buffer.push_str(stmt.as_str())
        }

        buffer
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.tree())
    }
}

#[enum_dispatch(Statement, Node)]
#[derive(Debug)]
pub enum Statements {
    LetStmt(LetStatement),
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_str(&self) -> &str {
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

    fn as_str(&self) -> &str {
        todo!()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
