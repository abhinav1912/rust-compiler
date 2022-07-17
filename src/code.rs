use std::{vec, fmt};

pub type Instructions = Vec<u8>;

pub fn make(op_code: OpCode) -> Instructions {
    return vec![op_code as u8]
}

pub fn make_u8(op_code: OpCode, operand: u8) -> Instructions {
    vec![op_code as u8, operand]
}

pub fn make_u16(op_code: OpCode, operand: u16) -> Instructions {
    let bytes = u16::to_be_bytes(operand);
    vec![op_code as u8, bytes[0], bytes[1]]
}

pub fn make_u16_u8(op_code: OpCode, first: u16, second: u8) -> Instructions {
    let bytes = u16::to_be_bytes(first);
    vec![op_code as u8, bytes[0], bytes[1], second]
}

pub fn print_instructions(insts: &[u8]) -> String {
    let mut result = String::new();
    let mut i = 0;
    while i < insts.len() {
        let op_code = insts[i];
        if let Some(def) = lookup_definition(op_code) {
            if i > 0 {
                result.push('\n');
            }
            result.push_str(&format!("{:04} ", i));
            i += 1;
            let (operands, offset) = read_operands(&def, insts, i);
            result.push_str(&def.name);
            for operand in operands {
                result.push_str(&format!(" {}", operand));
            }
            i += offset;
        } else {
            // TODO: Return result?
            return "".to_string();
        }
    }
    result
}

fn read_operands(def: &Definition, insts: &[u8], start: usize) -> (Vec<usize>, usize) {
    let mut offset = 0;
    let mut operands = Vec::with_capacity(def.widths.len());
    for width in &def.widths {
        match width {
            2 => {
                operands.push(read_uint16(insts, start + offset) as usize);
            }
            1 => {
                operands.push(insts[start + offset] as usize);
            }
            _ => {}
        }
        offset += width;
    }
    (operands, offset)
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
    println!("ccc {:?}, {}", instructions, start);
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

impl Constant {
    pub fn type_name(&self) -> &str {
        match self {
            Constant::Integer(_) => "INTEGER",
            Constant::Float(_) => "FLOAT",
            Constant::String(_) => "STRING",
        }
    }
}