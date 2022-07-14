use std::{cell::RefCell, rc::Rc};

use crate::{ast::Program, code::Instructions, object::Object};

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Object>>>,
    pub instructions: Rc<RefCell<Vec<Instructions>>>
}

pub struct ByteCode {
    pub constants: Rc<RefCell<Vec<Object>>>,
    pub instructions: Rc<RefCell<Vec<Instructions>>>
}

impl Compiler {
    pub fn new() -> Self {
        return Compiler {
            constants: Rc::new(RefCell::new(vec![])),
            instructions: Rc::new(RefCell::new(vec![]))
        }
    }

    pub fn compile(mut self, program: &Program) {
        return 
    }

    pub fn bytecode(self) -> ByteCode {
        return ByteCode { constants: self.constants, instructions: self.instructions }
    }
}