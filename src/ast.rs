use std::{fmt, vec};

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