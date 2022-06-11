use std::{fmt, vec};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64)
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Expression(Expression)
}

pub struct Program {
    pub statements: Vec<Statement>
}