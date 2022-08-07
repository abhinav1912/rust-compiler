pub mod frame;
use std::{rc::Rc, fmt, cell::RefCell, collections::HashMap};

use crate::{object::{Object, EvalError, HashKey}, code::{Instructions, Constant, OpCode, self}, compiler::{ByteCode, CompileError}, ast::{Infix, Prefix}};

pub const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
pub const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct Vm {
    pub constants: Vec<Constant>,
    instructions: Instructions,
    stack: Vec<Rc<Object>>,
    globals: Rc<RefCell<Vec<Rc<Object>>>>,
    stack_pointer: usize,
    ins_pointer: usize
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

pub fn new_globals() -> Vec<Rc<Object>> {
    Vec::with_capacity(GLOBAL_SIZE)
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
        Vm::new_with_globals_store(bytecode, Rc::new(RefCell::new(new_globals())))
    }

    pub fn new_with_globals_store(
        bytecode: ByteCode,
        globals: Rc<RefCell<Vec<Rc<Object>>>>,
    ) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        // Pre-fill the stack so that we can easily put values with stack pointer.
        for _ in 0..STACK_SIZE {
            stack.push(Rc::new(NULL));
        }

        Vm {
            constants: bytecode.constants,
            stack,
            stack_pointer: 0,
            globals,
            instructions: bytecode.instructions,
            ins_pointer: 0,
        }
    }

    pub fn run(mut self) -> Result<Rc<Object>, VmError> {
        while self.ins_pointer < self.instructions.len() {
            let op = OpCode::from_byte(self.instructions[self.ins_pointer]);
            match op {
                Some(OpCode::Constant) => {
                    let const_index = code::read_uint16(&self.instructions, self.ins_pointer+1) as usize;
                    self.increment_pointer(2);
                    let len = self.constants.len();
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
                Some(OpCode::Sub) => self.execute_binary_operation(OpCode::Sub)?,
                Some(OpCode::Mul) => self.execute_binary_operation(OpCode::Mul)?,
                Some(OpCode::Div) => self.execute_binary_operation(OpCode::Div)?,
                Some(OpCode::True) => self.push(Rc::new(Object::Boolean(true)))?,
                Some(OpCode::False) => self.push(Rc::new(Object::Boolean(false)))?,
                Some(OpCode::Equal) => self.execute_comparison(OpCode::Equal)?,
                Some(OpCode::NotEqual) => self.execute_comparison(OpCode::NotEqual)?,
                Some(OpCode::GreaterThan) => self.execute_comparison(OpCode::GreaterThan)?,
                Some(OpCode::Bang) => {
                    let right = self.pop()?;
                    self.push(Rc::new(Object::Boolean(!right.is_truthy())))?;
                },
                Some(OpCode::Minus) => {
                    let right = self.pop()?;
                    match &*right {
                        Object::Integer(value) => {
                            self.push(Rc::new(Object::Integer(-value)))?;
                        },
                        Object::Float(value) => {
                            self.push(Rc::new(Object::Float(-value)))?;
                        },
                        obj => {
                            return Err(VmError::Eval(EvalError::UnknownPrefixOperator(
                                Prefix::Minus,
                                obj.clone(),
                            )));
                        }
                    }
                },
                Some(OpCode::Jump) => {
                    let pos = code::read_uint16(&self.instructions, self.ins_pointer + 1) as usize;
                    self.set_pointer(pos-1);
                },
                Some(OpCode::JumpIfNotTruthy) => {
                    let pos = code::read_uint16(&self.instructions, self.ins_pointer + 1) as usize;
                    self.increment_pointer(2);

                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        self.set_pointer(pos-1);
                    }
                },
                Some(OpCode::Null) => {
                    self.push(Rc::new(NULL))?;
                },
                Some(OpCode::SetGlobal) => {
                    let global_index = code::read_uint16(&self.instructions, self.ins_pointer + 1) as usize;
                    self.increment_pointer(2);

                    let popped = self.pop()?;
                    let mut globals = self.globals.borrow_mut();
                    if global_index == globals.len() {
                        globals.push(popped);
                    } else {
                        globals[global_index] = popped;
                    }
                },
                Some(OpCode::GetGlobal) => {
                    let global_index = code::read_uint16(&self.instructions, self.ins_pointer + 1) as usize;
                    self.increment_pointer(2);

                    let global = Rc::clone(&self.globals.borrow()[global_index]);
                    self.push(global)?;
                },
                Some(OpCode::Array) => {
                    let array_size = code::read_uint16(&self.instructions, self.ins_pointer + 1) as usize;
                    self.increment_pointer(2);
                    
                    let mut items = Vec::with_capacity(array_size);
                    for _ in 0..array_size {
                        items.push((*self.pop()?).clone());
                    }
                    items.reverse();
                    self.push(Rc::new(Object::Array(items)))?;
                },
                Some(OpCode::Hash) => {
                    let hash_size = code::read_uint16(&self.instructions, self.ins_pointer + 1) as usize;
                    self.increment_pointer(2);
                    
                    let mut items = HashMap::with_capacity(hash_size);
                    for _ in 0..hash_size {
                        let value = (*self.pop()?).clone();
                        let key = HashKey::from_object(&*self.pop()?)
                            .or_else(|e| Err(VmError::Eval(e)))?;
                        items.insert(key, value);
                    }
                    self.push(Rc::new(Object::Hash(items)))?;
                },
                Some(OpCode::Index) => {
                    let index = self.pop()?;
                    let object = self.pop()?;
                    match &*object {
                        Object::Array(values) => {
                            if let Object::Integer(i) = &*index {
                                let item = values.get(*i as usize).unwrap_or(&NULL).clone();
                                self.push(Rc::new(item))?;
                            } else {
                                return Err(VmError::Eval(EvalError::UnknownIndexOperator(
                                    (*object).clone(),
                                    (*index).clone(),
                                )));
                            }
                        },
                        Object::Hash(hash) => {
                            let key = match &*index {
                                Object::Integer(i) => HashKey::Integer(*i),
                                Object::String(s) => HashKey::String(s.clone()),
                                Object::Boolean(b) => HashKey::Boolean(*b),
                                _ => {
                                    return Err(VmError::Eval(EvalError::UnknownIndexOperator(
                                        (*object).clone(),
                                        (*index).clone(),
                                    )));
                                }
                            };
                            let value = hash.get(&key).unwrap_or(&NULL);
                            self.push(Rc::new(value.clone()))?;
                        },
                        _ => {
                            return Err(VmError::Eval(EvalError::UnknownIndexOperator(
                                (*object).clone(),
                                (*index).clone(),
                            )));
                        }
                    }
                }
                _ => return Err(VmError::UnknownOpCode(self.instructions[self.ins_pointer]))
            }
            self.increment_pointer(1);
        }
        self.stack
            .get(self.stack_pointer)
            .map(|o| Rc::clone(o))
            .ok_or(VmError::StackEmpty)
    }

    fn push(&mut self, obj: Rc<Object>) -> Result<(), VmError> {
        if self.stack_pointer >= STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        self.stack[self.stack_pointer] = obj;
        self.stack_pointer += 1;
        Ok(())
    }

    fn pop(&mut self) -> Result<Rc<Object>, VmError> {
        let popped = self.stack.get(self.stack_pointer - 1);
        self.stack_pointer -= 1;
        popped.map(|o| Rc::clone(o)).ok_or(VmError::StackEmpty)
    }

    fn execute_binary_operation(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_integer_binary_operation(op_code, *l, *r)
            },
            (Object::Integer(l), Object::Float(r)) => {
                self.execute_float_binary_operation(op_code, *l as f64, *r)
            },
            (Object::Float(l), Object::Integer(r)) => {
                self.execute_float_binary_operation(op_code, *l, *r as f64)
            },
            (Object::Float(l), Object::Float(r)) => {
                self.execute_float_binary_operation(op_code, *l as f64, *r)
            },
            (Object::String(l), Object::String(r)) => {
                self.execute_binary_string_operation(op_code, l, r)
            },
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

    fn execute_float_binary_operation(
        &mut self,
        op_code: OpCode,
        left: f64,
        right: f64
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
        self.push(Rc::new(Object::Float(result)))
    }

    fn execute_binary_string_operation(
        &mut self,
        op_code: OpCode,
        left: &str,
        right: &str
    ) -> Result<(), VmError> {
        match op_code {
            OpCode::Add => {
                let result = format!("{}{}", left, right);
                self.push(Rc::new(Object::String(result)))
            }
            OpCode::Sub | OpCode::Mul | OpCode::Div => {
                Err(VmError::Eval(EvalError::UnknownInfixOperator(
                    infix_from_op_code(op_code).expect("not string binary operation"),
                    Object::String(left.to_string()),
                    Object::String(right.to_string()),
                )))
            }
            _ => {
                panic!("not string binary operation: {:?}", op_code);
            }
        }
    }

    fn execute_comparison(&mut self, op_code: OpCode) -> Result<(), VmError> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (&*left, &*right) {
            (Object::Integer(l), Object::Integer(r)) => {
                match op_code {
                    OpCode::Equal => self.push(Rc::new(Object::Boolean(l == r))),
                    OpCode::NotEqual => self.push(Rc::new(Object::Boolean(l != r))),
                    OpCode::GreaterThan => self.push(Rc::new(Object::Boolean(l > r))),
                    _ => {
                        panic!("unknown operator: {:?}", op_code);
                    }
                }
            },
            (Object::Boolean(l), Object::Boolean(r)) => {
                match op_code {
                    OpCode::Equal => self.push(Rc::new(Object::Boolean(l == r))),
                    OpCode::NotEqual => self.push(Rc::new(Object::Boolean(l != r))),
                    OpCode::GreaterThan => Err(VmError::Eval(EvalError::UnknownInfixOperator(
                        Infix::Gt,
                        Object::Boolean(*l),
                        Object::Boolean(*r),
                    ))),
                    _ => {
                        // This happens only when this vm is wrong.
                        panic!("unknown operator: {:?}", op_code);
                    }
                }
            }
            (l, r) => {
                let infix = infix_from_op_code(op_code).expect("not comparison");
                Err(VmError::Eval(EvalError::TypeMismatch(
                    infix,
                    l.clone(),
                    r.clone(),
                )))
            }
        }
    }

    fn set_pointer(&mut self, pos: usize) {
        self.ins_pointer = pos;
    }

    fn increment_pointer(&mut self, increment: usize) {
        self.ins_pointer += increment;
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