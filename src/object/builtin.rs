use crate::object::Object;

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
    
];

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