use std::rc::Rc;

use crate::{object::Object, code::{Instructions, Constant}, compiler::ByteCode};

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
}