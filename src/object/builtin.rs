use crate::object::{Object, EvalResult, EvalError, assert_argument_count};

pub struct Builtin {
    pub name: &'static str,
    pub builtin: Object
}

macro_rules! builtin {
    ($name: ident) => {
        Builtin {
            name: stringify!($name),
            builtin: Object::Builtin($name)
        }
    };
}

pub const BUILTINS: &[Builtin] = &[
    builtin!(len),
    builtin!(first),
    builtin!(last),
    builtin!(rest),
    builtin!(push)
];

fn len(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::String(value) => Ok(Object::Integer(value.len() as i64)),
        Object::Array(array) => Ok(Object::Integer(array.len() as i64)),
        _ => Err(EvalError::UnsupportedArguments("len".to_string(), arguments))
    }
}

fn first(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::Array(array) => {
            Ok(match array.first() {
                Some(element) => element.clone(),
                None => Object::Null
            })
        },
        _ => Err(EvalError::UnsupportedArguments("first".to_string(), arguments))
    }
}

fn last(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::Array(array) => {
            Ok(match array.last() {
                Some(element) => element.clone(),
                None => Object::Null
            })
        },
        _ => Err(EvalError::UnsupportedArguments("last".to_string(), arguments))
    }
}

fn rest(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::Array(array) => {
            if array.len() < 2 {
                return Ok(Object::Null)
            }
            let slice = array[1..].to_vec();
            return Ok(Object::Array(slice))
        },
        _ => Err(EvalError::UnsupportedArguments("rest".to_string(), arguments))
    }
}

fn push(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(2, &arguments)?;
    match &arguments[0] {
        Object::Array(array) => {
            let mut values = array.clone();
            values.push(arguments[1].clone());
            Ok(Object::Array(values))
        },
        _ => Err(EvalError::UnsupportedArguments("push".to_string(), arguments))
    }
}

pub fn lookup(name: &str) -> Option<Object> {
    if name == "null" {
        return Some(Object::Null)
    }
    for builtin in BUILTINS {
        if builtin.name == name {
            return Some(builtin.builtin.clone())
        }
    }
    None
}