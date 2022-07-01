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