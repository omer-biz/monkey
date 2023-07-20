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
    Integ(IntegerLiteral),
    PrExp(PrefixExpression),
    InExp(InfixExpression),
    Boole(Boolean),
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

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: f64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: char,
    pub right: Box<Expressions>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut buffer = String::new();

        buffer.push('(');
        buffer.push(self.operator);
        buffer.push_str(&self.right.as_string());
        buffer.push(')');

        buffer
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expressions>,
    pub operator: String,
    pub right: Box<Expressions>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        todo!()
    }

    fn as_string(&self) -> String {
        let mut buffer = String::new();

        buffer.push('(');
        buffer.push_str(&self.left.as_string());
        buffer.push(' ');
        buffer.push_str(&self.operator);
        buffer.push(' ');
        buffer.push_str(&self.right.as_string());
        buffer.push(')');

        buffer
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        self.token_literal().to_string()
    }
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}
