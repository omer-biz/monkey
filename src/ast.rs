use std::fmt::Display;

use enum_dispatch::enum_dispatch;

use crate::token::Token;

#[enum_dispatch]
pub trait Node {
    fn token_literal(&self) -> &str;
    fn as_string(&self) -> String;
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
    pub value: Expressions,
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
    pub fn as_string(&self) -> String {
        let mut buffer: String = String::new();

        for stmt in &self.statements {
            buffer.push_str(&stmt.as_string())
        }

        buffer
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[enum_dispatch(Statement, Node)]
#[derive(Debug)]
pub enum Statements {
    LetStmt(LetStatement),
    RetStmt(ReturnStatement),
    ExpStmt(ExpressionStatement),
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&format!(
            "{} {} = {}",
            self.token_literal(),
            &self.name.as_string(),
            self.value.as_string(),
        ));

        buffer.push(';');

        buffer
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

    fn as_string(&self) -> String {
        self.value.to_string()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expressions>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&format!("{} ", self.token_literal()));

        if self.return_value.is_some() {
            buffer.push_str(
                &self
                    .return_value
                    .as_ref()
                    .expect("no return value")
                    .as_string(),
            );
        }

        buffer.push(';');

        buffer
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {
        todo!()
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expressions,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        self.expression.as_string()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}
