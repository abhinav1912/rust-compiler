use std::{vec, fmt};

pub type Instructions = Vec<u8>;

pub fn make(op_code: OpCode) -> Instructions {
    return vec![op_code as u8]
}

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

pub fn read_uint16(instructions: &[u8], start: usize) -> u16 {
    u16::from_be_bytes([instructions[start], instructions[start+1]])
}

impl OpCode {
    pub fn u16(i: u16) -> Vec<u8> {
        let bytes = i.to_be_bytes();
        vec![bytes[0], bytes[1]]
    }
}

pub struct Definition {
    pub name: String,
    pub widths: Vec<usize>
}

fn lookup_definition(byte: u8) -> Option<Definition> {
    OpCode::from_byte(byte).map(|op_code| match op_code {
        OpCode::Constant => Definition {
            name: "OpConstant".to_string(),
            widths: vec![2]
        },
        _ => Definition {
            name: "Not implemented".to_string(),
            widths: vec![0]
        }
    })
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Integer(i64),
    Float(f64),
    String(String)    
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Integer(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
            Constant::String(value) => write!(f, "\"{}\"", value)
        }
    }
}