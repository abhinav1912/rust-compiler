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
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(OpCode::Pop);
            },
            _ => return Err(CompileError::CompilingNotImplemented)
        }
        Ok(())
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompileError> {
        match expression {
            Expression::Infix(infix, left, right) => {
                match infix {
                    Infix::Lt => {
                        // Convert `a < b` to `a > b` to keep the instruction set smaller.
                        // TODO: This is an issue if expressions have side effects though.
                        self.compile_expression(right)?;
                        self.compile_expression(left)?;
                    }
                    _ => {
                        self.compile_expression(left)?;
                        self.compile_expression(right)?;
                    }
                }
                match infix {
                    Infix::Plus => {
                        self.emit(OpCode::Add);
                    }
                    Infix::Minus => {
                        self.emit(OpCode::Sub);
                    }
                    Infix::Asterisk => {
                        self.emit(OpCode::Mul);
                    }
                    Infix::Slash => {
                        self.emit(OpCode::Div);
                    }
                    Infix::Eq => {
                        self.emit(OpCode::Equal);
                    }
                    Infix::NotEq => {
                        self.emit(OpCode::NotEqual);
                    }
                    Infix::Gt | Infix::Lt => {
                        self.emit(OpCode::GreaterThan);
                    }
                }
            }
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

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::ast::Program;
    use crate::code::{
        make, make_u16, make_u16_u8, make_u8, Constant, print_instructions,
        Instructions, OpCode,
    };
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn compile() {
        test_compile(vec![
            (
                "1 + 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Add),
                ],
            ),
        ]);
    }

    fn test_compile(tests: Vec<(&str, Vec<Constant>, Vec<Instructions>)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let program = parse(input);

            let compiler = Compiler::new();
            let bytecode = match compiler.compile(&program) {
                Ok(bytecode) => bytecode,
                Err(error) => panic!("failed to compile input `{}`: {}", input, error),
            };

            // Compare instructions.
            assert_eq!(
                print_instructions(&bytecode.instructions),
                print_instructions(&expected_instructions.concat()),
                "\nfor `{}`",
                input
            );

            if bytecode.constants.len() != expected_constants.len() {
                assert_eq!(bytecode.constants, expected_constants, "\nfor {}", input);
            }

            // Pretty-print instructions in error messages
            let pairs = bytecode.constants.iter().zip(expected_constants.iter());
            for (i, (constant, expected_constant)) in pairs.enumerate() {
                match (constant, expected_constant) {
                    _ => {
                        assert_eq!(
                            constant, expected_constant,
                            "\nconstant (index {}) for {}",
                            i, input
                        );
                    }
                }
            }
        }
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new_parser(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        program
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.len() > 0 {
            panic!(
                "for input '{}', got parser errors: {:?}",
                parser.input(),
                errors
            );
        }
    }
}
