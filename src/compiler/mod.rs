use std::{cell::RefCell, rc::Rc, fmt};

use crate::{ast::{Program, Infix, Statement}, code::Instructions, object::Object};

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

    pub fn compile(mut self, program: &Program) -> Result<ByteCode, CompileError> {
        for statement in &program.statements {
            self.compile_statement(statement)?;
        }
        Ok(self.bytecode())
    }

    pub fn compile_statement(&mut self, statement: &Statement) -> Result<(), CompileError> {
        return Err(CompileError::TooManyParams)
    }

    pub fn bytecode(self) -> ByteCode {
        return ByteCode { constants: self.constants, instructions: self.instructions }
    }
}

#[derive(Debug)]
pub enum CompileError {
    UnknownOperator(Infix),
    UndefinedVariable(String),
    TooManyConstants,
    TooManyParams,
    TooManyLocals,
    TooManyFrees,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::UnknownOperator(infix) => write!(f, "unknown operator: {}", infix),
            CompileError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
            CompileError::TooManyConstants => write!(f, "too many constants"),
            CompileError::TooManyParams => write!(f, "too many parameters for a function"),
            CompileError::TooManyLocals => write!(f, "too many local bindings in a function"),
            CompileError::TooManyFrees => write!(f, "too many free bindings in a function"),
        }
    }
}