pub mod environment;
use std::fmt;
use crate::ast::{Prefix, Infix};

pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Null
}

#[derive(Debug)]
pub enum EvalError {
    UnknownPrefixOperator(Prefix, Object),
    UnknownInfixOperator(Infix, Object, Object),
    TypeMismatch(Infix, Object, Object)
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(value) => *value,
            Object::Null => false,
            _ => true
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(_) => todo!(),
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }    
}