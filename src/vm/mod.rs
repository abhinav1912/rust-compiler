use std::{rc::Rc, fmt};

use crate::{object::{Object, EvalError}, code::{Instructions, Constant, OpCode, self}, compiler::{ByteCode, CompileError}, ast::Infix};

pub const STACK_SIZE: usize = 2048;
pub const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Vm {
    pub constants: Vec<Constant>,
    instructions: Instructions,
    stack: Vec<Rc<Object>>,
    pointer: usize
}

#[derive(Debug)]
pub enum VmError {
    UnknownOpCode(u8),
    InvalidConstIndex(usize, usize),
    StackOverflow,
    StackEmpty,
    NotFunction(Constant),
    Eval(EvalError),
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VmError::UnknownOpCode(op_code) => write!(f, "unknown op code: {}", op_code),
            VmError::InvalidConstIndex(given, length) => {
                write!(f, "invalid const index: {} / {}", given, length)
            }
            VmError::StackOverflow => write!(f, "stack overflow"),
            VmError::StackEmpty => write!(f, "stack empty"),
            VmError::NotFunction(con) => write!(f, "not a function: {}", con.type_name()),
            VmError::Eval(eval_error) => write!(f, "{}", eval_error),
        }
    }
}

impl Vm {
    pub fn new(bytecode: ByteCode) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        for _ in 0..STACK_SIZE {
            stack.push(Rc::new(NULL));
        }
        return Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack,
            pointer: 0
        }
    }

    pub fn run(mut self) -> Result<Rc<Object>, VmError> {
        let mut pointer = 0;
        while pointer < self.instructions.len() {
            let op = OpCode::from_byte(self.instructions[pointer]);
            println!("fk {:?}, {}", self.instructions, pointer);
            match op {
                Some(OpCode::Constant) => {
                    let const_index = code::read_uint16(&self.instructions, pointer+1) as usize;
                    pointer += 2;

                    let len = self.constants.len();
                    println!("rrr {}, {}", const_index, len);
                    if const_index < len {
                        let constant = Object::from_constant(&self.constants[const_index]);
                        self.push(Rc::new(constant))?;
                    } else {
                        return Err(VmError::InvalidConstIndex(const_index, len));
                    }
                },
                Some(OpCode::Pop) => {
                    self.pop()?;
                },
                Some(OpCode::Add) => self.execute_binary_operation(OpCode::Add)?,
                _ => return Err(VmError::UnknownOpCode(self.instructions[pointer]))
            }
            pointer += 1;
        }
        self.stack
            .get(self.pointer)
            .map(|o| Rc::clone(o))
            .ok_or(VmError::StackEmpty)
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.pointer >= STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        self.stack[self.pointer] = obj;
        self.pointer += 1;
        Ok(())
    }

    fn pop(&mut self) -> Result<Rc<Object>, VmError> {
        let popped = self.stack.get(self.pointer - 1);
        self.pointer -= 1;
        popped.map(|o| Rc::clone(o)).ok_or(VmError::StackEmpty)
    }

    fn execute_binary_operation(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_integer_binary_operation(op_code, *l, *r)
            }
            (l, r) => {
                let infix = infix_from_op_code(op_code).expect("not binary operation");
                Err(VmError::Eval(EvalError::TypeMismatch(
                    infix,
                    l.clone(),
                    r.clone(),
                )))
            }
        }
    }

    fn execute_integer_binary_operation(
        &mut self,
        op_code: OpCode,
        left: i64,
        right: i64,
    ) -> Result<(), VmError> {
        let result = match op_code {
            OpCode::Add => left + right,
            OpCode::Sub => left - right,
            OpCode::Mul => left * right,
            OpCode::Div => left / right,
            _ => {
                // This happens only when this vm is wrong.
                panic!("not integer binary operation: {:?}", op_code);
            }
        };

        self.push(Rc::new(Object::Integer(result)))
    }

}

fn infix_from_op_code(op_code: OpCode) -> Option<Infix> {
    match op_code {
        OpCode::Add => Some(Infix::Plus),
        OpCode::Sub => Some(Infix::Minus),
        OpCode::Mul => Some(Infix::Asterisk),
        OpCode::Div => Some(Infix::Slash),
        OpCode::Equal => Some(Infix::Eq),
        OpCode::NotEqual => Some(Infix::NotEq),
        OpCode::GreaterThan => Some(Infix::Gt),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser, compiler::Compiler};

    use super::Vm;

    #[test]
    fn integer() {
        expect_values(vec![
            ("1", "1"),
        ]);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let vm = make_vm(input);
            match vm.run() {
                Ok(obj) => {
                    assert_eq!(&obj.to_string(), expected, "for `{}`", input);
                }
                Err(err) => {
                    panic!("error on vm for `{}`: {}", input, err);
                }
            }
        }
    }

    fn make_vm(input: &str) -> Vm {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();
        let errors = parser.errors;
        if errors.len() > 0 {
            panic!("for input '{}', got parser errors: {:?}", input, errors);
        }

        let compiler = Compiler::new();
        let bytecode = match compiler.compile(&program) {
            Ok(bytecode) => bytecode,
            Err(err) => {
                panic!("error on compile for `{}`: {}", input, err);
            }
        };
        println!("ttt {:?}", bytecode.constants);
        Vm::new(bytecode)
    }
}