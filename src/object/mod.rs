pub mod environment;
pub mod builtin;

use std::{fmt, rc::Rc, cell::RefCell, collections::HashMap};
use crate::{ast::{Prefix, Infix, BlockStatement}, code::{Constant, CompiledFunction, self}};

use self::environment::Environment;

pub type EvalResult = Result<Object, EvalError>;
pub type BuiltinFunction = fn(Vec<Object>) -> EvalResult;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Function(Vec<String>, BlockStatement, Rc<RefCell<Environment>>),
    String(String),
    Builtin(BuiltinFunction),
    Array(Vec<Object>),
    Float(f64),
    Hash(HashMap<HashKey, Object>),
    Null,
    CompiledFunction(CompiledFunction),
    Closure(Closure)
}

#[derive(Debug)]
pub enum EvalError {
    UnknownPrefixOperator(Prefix, Object),
    UnknownInfixOperator(Infix, Object, Object),
    TypeMismatch(Infix, Object, Object),
    IdentifierNotFound(String),
    NotCallable(Object),
    WrongArgumentCount {expected: usize, given: usize},
    UnsupportedArguments(String, Vec<Object>),
    UnknownIndexOperator(Object, Object),
    UnsupportedHashKey(Object)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl HashKey {
    pub fn from_object(obj: &Object) -> Result<HashKey, EvalError> {
        match obj {
            Object::Integer(value) => Ok(HashKey::Integer(*value)),
            Object::Boolean(value) => Ok(HashKey::Boolean(*value)),
            Object::String(value) => Ok(HashKey::String(value.to_string())),
            _ => Err(EvalError::UnsupportedHashKey(obj.clone())),
        }
    }
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
            Object::Function(_, _, _) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Float(_) => "FLOAT",
            Object::Hash(_) => "HASH",
            Object::CompiledFunction(_) => "COMPILED_FUNCTION",
            Object::Closure(_) => "CLOSURE",
        }
    }

    pub fn from_constant(constant: &Constant) -> Object {
        match constant {
            Constant::Integer(value) => Object::Integer(*value),
            Constant::Float(value) => Object::Float(*value),
            Constant::String(value) => Object::String(value.clone()),
            Constant::CompiledFunction(value) => Object::CompiledFunction(value.clone()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", *value),
            Object::Function(params, body, _) => write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body),
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Array(values) => {
                let value_list = values
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", value_list)
            },
            Object::Float(value) => write!(f, "{}", value),
            Object::Hash(pairs) => {
                // Print keys with a stable order for testing.
                let mut items = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>();
                items.sort();
                write!(f, "{{{}}}", items.join(", "))
            },
            Object::CompiledFunction(cf) => write!(f, "{}", cf),
            Object::Closure(closure) => {
                let free_list = closure
                    .free
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(
                    f,
                    "closure ({}) ({}): {}",
                    closure.func.num_locals,
                    free_list,
                    code::print_instructions(&closure.func.instructions),
                )
            }
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::TypeMismatch(infix, left, right) => write!(
                f,
                "type mismatch: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            EvalError::UnknownPrefixOperator(prefix, right) => {
                write!(f, "unknown operator: {}{}", prefix, right.type_name())
            }
            EvalError::UnknownInfixOperator(infix, left, right) => write!(
                f,
                "unknown operator: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            EvalError::IdentifierNotFound(string) => write!(f, "identifier not found: {}", string),
            EvalError::NotCallable(object) => write!(
                f,
                "not a closure or builtin function: {}",
                object.type_name()
            ),
            EvalError::WrongArgumentCount { expected, given } => write!(
                f,
                "wrong number of arguments: expected {}, given {}",
                expected, given
            ),
            EvalError::UnsupportedArguments(name, arguments) => write!(
                f,
                "unsupported arguments to `{}`: {}",
                name,
                arguments
                    .iter()
                    .map(|a| a.type_name())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ),
            EvalError::UnknownIndexOperator(left, index) => write!(
                f,
                "unknown operator: {}[{}]",
                left.type_name(),
                index.type_name()
            ),
            EvalError::UnsupportedHashKey(key) => {
                write!(f, "unusable as hash key: {}", key.type_name())
            }
        }
    }    
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::Integer(value) => write!(f, "{}", value),
            HashKey::String(value) => write!(f, "\"{}\"", value),
            HashKey::Boolean(value) => write!(f, "{}", value),
        }
    }
}

pub fn assert_argument_count(expected: usize, arguments: &[Object]) -> Result<(), EvalError> {
    if arguments.len() != expected {
        return Err(EvalError::WrongArgumentCount { 
            expected: expected,
            given: arguments.len() 
        })
    }
    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub func: CompiledFunction,
    pub free: Vec<Rc<Object>>,
}