use std::{cell::RefCell, rc::Rc, fmt};

use crate::{ast::{Program, Infix, Statement, Expression}, code::{Instructions, Constant, OpCode}, object::Object};

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Constant>>>,
    pub instructions: Rc<RefCell<Instructions>>
}

pub struct ByteCode {
    pub constants: Vec<Constant>,
    pub instructions: Instructions
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
        match statement {
            Statement::Expression(expression) => self.compile_expression(expression),
            _ => Err(CompileError::CompilingNotImplemented)
        }
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompileError> {
        match expression {
            Expression::IntegerLiteral(value) => {
                let constant = Constant::Integer(*value);
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            },
            _ => return Err(CompileError::CompilingNotImplemented)
        }
        Ok(())
    }

    pub fn bytecode(self) -> ByteCode {
        return ByteCode { constants: self.constants.borrow().clone(), instructions: self.instructions.borrow().clone() }
    }

    fn add_constant(&mut self, constant: Constant) -> Result<u16, CompileError> {
        let const_index = self.constants.borrow_mut().len();
        self.constants.borrow_mut().push(constant);
        Ok(const_index as u16)
    }

    fn emit(&mut self, op_code: OpCode) -> usize {
        self.emit_with_operands(op_code, vec![])
    }

    fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        let pos = self.instructions.borrow().len();
        self.instructions.borrow_mut().push(op_code as u8);
        self.instructions.borrow_mut().extend(operands);
        return pos
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
    CompilingNotImplemented
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
            CompileError::CompilingNotImplemented => write!(f, "Not implemeneted"),
        }
    }
}