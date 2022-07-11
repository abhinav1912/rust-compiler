pub type Instruction = Vec<u8>;

macro_rules! byte_enum {
    (@step $_idx:expr, $name:ident, $_byte:ident, []) => {
        None as Option<$name>
    };
    (@step $idx:expr, $name:ident, $byte:ident, [$head:ident, $($tail:ident,)*]) => {
        if $byte == $idx {
            return Some($name::$head);
        }
        byte_enum!(@step $idx + 1u8, $name, $byte, [$($tail,)*]);
    };
    ($name:ident, [$($var: ident),+]) => {
        #[derive(Debug, Clone, Copy, PartialEq)]
        #[repr(u8)]
        pub enum $name {
            $($var,)+
        }
        impl $name {
            pub fn from_byte(byte: u8) -> Option<$name> {
                byte_enum!(@step 0u8, $name, byte, [$($var,)+]);
                None
            }
        }
    };
}

byte_enum!(
    OpCode,
    [
        Constant,
        Pop,
        Add,
        Sub,
        Mul,
        Div,
        True,
        False,
        Equal,
        NotEqual,
        GreaterThan,
        Minus,
        Bang,
        JumpIfNotTruthy,
        Jump,
        Null,
        GetGlobal,
        SetGlobal,
        Array,
        Hash,
        Index,
        Call,
        ReturnValue,
        Return,
        GetLocal,
        SetLocal,
        GetBuiltin,
        Closure,
        GetFree
    ]
);

pub struct Definition {
    name: String,
    widths: Vec<usize>
}