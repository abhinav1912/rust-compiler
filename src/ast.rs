use std::{fmt, Vec};

pub enum Expression {
    Identifier(String)
}

pub enum Statement {
    Let(String, Expression),
    Expression(Expression)
}

pub struct Program {
    pub statements: Vec<Statement>
}