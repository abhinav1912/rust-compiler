use std::{rc::Rc};

use crate::{object::Object, code::{Instructions, Constant, OpCode, self}, compiler::{ByteCode, CompileError}};

pub const STACK_SIZE: usize = 2048;
pub const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Vm {
    pub constants: Vec<Constant>,
    instructions: Instructions,
    stack: Vec<Rc<Object>>,
    pointer: usize
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
            stack: vec![],
            pointer: 0
        }
    }

    pub fn run(&mut self) -> Result<(), CompileError> {
        for pointer in 0.. self.instructions.len() {
            let op = OpCode::from_byte(self.instructions[pointer]);
            match op {
                Some(OpCode::Constant) => {
                    let const_index = code::read_uint16(&self.instructions, self.pointer+1) as usize;
                    self.pointer += 2;

                    let len = self.constants.len();
                    if const_index < len {
                        let constant = Object::from_constant(&self.constants[const_index]);
                        self.push(Rc::new(constant))?;
                    } else {
                        return Err(CompileError::TooManyConstants) // TODO: Fix error
                    }
                }, 
                _ => return Err(CompileError::CompilingNotImplemented)
            }
        }
        Ok(())
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), CompileError> {
        if self.pointer >= STACK_SIZE {
            return Err(CompileError::TooManyConstants)
        }
        self.stack[self.pointer] = obj;
        self.pointer += 1;
        Ok(())
    }
}