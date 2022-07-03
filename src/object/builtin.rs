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
    builtin!(len)
];

fn len(arguments: Vec<Object>) -> EvalResult {
    assert_argument_count(1, &arguments)?;
    match &arguments[0] {
        Object::String(value) => Ok(Object::Integer(value.len() as i64)),
        Object::Array(array) => Ok(Object::Integer(array.len() as i64)),
        _ => Err(EvalError::UnsupportedArguments("len".to_string(), arguments))
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