use std::{fmt, vec};

pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64)
}

pub enum Statement {
    Let(String, Expression),
    Expression(Expression)
}

pub struct Program {
    pub statements: Vec<Statement>
}