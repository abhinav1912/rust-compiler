use std::fmt;

pub enum Expression {
    Identifier(String)
}

pub enum Statement {
    Let(String, Expression),
    Expression(Expression)
}