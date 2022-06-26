use std::fmt;

pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null
}

#[derive(Debug)]
pub enum EvalError {

}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(value) => *value,
            Object::Null => false,
            _ => true
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => writeln!(f, "{}", value),
            Object::Boolean(value) => writeln!(f, "{}", value),
            Object::Null => writeln!(f, "{}", "null"),
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }    
}