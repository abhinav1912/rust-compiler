pub mod symbol_table;

use std::{cell::RefCell, rc::Rc, fmt, mem};

use crate::{ast::{Program, Infix, Statement, Expression, BlockStatement}, code::{Instructions, Constant, OpCode, self}, object::Object};

use self::symbol_table::{SymbolTable, SymbolScope, Symbol};

const TENTATIVE_JUMP_POS: u16 = 9999;

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Constant>>>,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
    symbol_table: Rc<RefCell<SymbolTable>>
}

#[derive(Debug)]
pub struct ByteCode {
    pub constants: Vec<Constant>,
    pub instructions: Instructions
}

pub struct EmittedInstruction {
    pub op_code: OpCode,
    pub position: usize,
}

#[derive(Default)]
pub struct CompilationScope {
    pub instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::new();
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
        return Compiler {
            constants: Rc::new(RefCell::new(vec![])),
            scopes: vec![main_scope],
            scope_index: 0,
            symbol_table
        }
    }

    pub fn new_with_state(symbol_table: Rc<RefCell<SymbolTable>>, constants: Rc<RefCell<Vec<Constant>>>) -> Self {
        let main_scope = CompilationScope::new();
        Compiler {
            constants,
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
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
            Statement::Let(name, exp) => {
                let symbol = *self.symbol_table.borrow_mut().define(name);
                self.compile_expression(exp)?;

                match symbol.scope {
                    SymbolScope::Global => {
                        self.emit_with_operands(OpCode::SetGlobal, OpCode::u16(symbol.index));
                    },
                    SymbolScope::Local => todo!(),
                }

            }
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
            Expression::FloatLiteral(value) => {
                let constant = Constant::Float(*value);
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            Expression::Boolean(value) => {
                let op_code = if *value {OpCode::True} else {OpCode::False};
                self.emit(op_code);
            },
            Expression::Prefix(prefix, expression) => {
                self.compile_expression(expression)?;
                match prefix {
                    crate::ast::Prefix::Bang => {
                        self.emit(OpCode::Bang);
                    },
                    crate::ast::Prefix::Minus => {
                        self.emit(OpCode::Minus);
                    },
                }
            },
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;
                let jump_not_truthy_pos = self.emit_with_operands(OpCode::JumpIfNotTruthy, OpCode::u16(TENTATIVE_JUMP_POS));
                self.compile_block_statement(consequence)?;
                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_pop();
                }
                
                let jump_pos = self.emit_with_operands(OpCode::Jump, OpCode::u16(TENTATIVE_JUMP_POS));
                self.replace_instruction(
                    jump_not_truthy_pos,
                    code::make_u16(OpCode::JumpIfNotTruthy, self.current_instructions().len() as u16)
                );

                match alternative {
                    Some(alt) => {
                        self.compile_block_statement(alt)?;
                        if self.last_instruction_is(OpCode::Pop) {
                            self.remove_last_pop();
                        }
                    }
                    None => {
                        self.emit(OpCode::Null);
                    }
                }
                self.replace_instruction(
                    jump_pos,
                    code::make_u16(OpCode::Jump, self.current_instructions().len() as u16)
                );
            },
            Expression::Identifier(name) => {
                let symbol = {
                    match self.symbol_table.borrow_mut().resolve(name) {
                        Some(symbol) => symbol,
                        None => return Err(CompileError::UndefinedVariable(name.to_string())),
                    }
                };
                self.load_symbol(symbol);
            },
            Expression::StringLiteral(val) => {
                let const_index = self.add_constant(Constant::String(val.to_string()))?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            },
            Expression::Array(expressions) => {
                for expression in expressions {
                    self.compile_expression(expression)?;
                }
                self.emit_with_operands(OpCode::Array, OpCode::u16(expressions.len() as u16));
            }
            _ => return Err(CompileError::CompilingNotImplemented)
        }
        Ok(())
    }

    pub fn bytecode(self) -> ByteCode {
        return ByteCode { constants: self.constants.borrow().clone(), instructions: self.current_instructions().clone() }
    }

    fn add_constant(&mut self, constant: Constant) -> Result<u16, CompileError> {
        let const_index = self.constants.borrow_mut().len();
        if const_index >= 0xffff {
            return Err(CompileError::TooManyConstants);
        }
        self.constants.borrow_mut().push(constant);
        Ok(const_index as u16)
    }

    fn emit(&mut self, op_code: OpCode) -> usize {
        self.emit_with_operands(op_code, vec![])
    }

    fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        self.scopes[self.scope_index].emit_with_operands(op_code, operands)
    }

    fn compile_block_statement(&mut self, block: &BlockStatement) -> Result<(), CompileError> {
        for statement in &block.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            SymbolScope::Global => {
                self.emit_with_operands(OpCode::GetGlobal, OpCode::u16(symbol.index));
            },
            SymbolScope::Local => todo!(),
        }
    }

    fn last_instruction_is(&self, op_code: OpCode) -> bool {
        self.scopes[self.scope_index].last_instruction_is(op_code)
    }

    fn remove_last_pop(&mut self) {
        self.scopes[self.scope_index].remove_last_pop()
    }

    fn replace_instruction(&mut self, pos: usize, instruction: Instructions) {
        self.scopes[self.scope_index].replace_instruction(pos, instruction)
    }

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
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

impl CompilationScope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        let pos = self.instructions.len();
        self.instructions.push(op_code as u8);
        self.instructions.extend(operands);
        self.set_last_instruction(op_code, pos);
        return pos
    }

    pub fn last_instruction_is(&self, op_code: OpCode) -> bool {
        match &self.last_instruction {
            Some(emitted) => emitted.op_code == op_code,
            None => false,
        }
    }

    pub fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for (i, byte) in new_instruction.iter().enumerate() {
            let offset = pos + i;
            if offset < self.instructions.len() {
                self.instructions[offset] = *byte;
            } else {
                self.instructions.push(*byte);
            }
        }
    }

    pub fn remove_last_pop(&mut self) {
        if let Some(emitted) = &self.last_instruction {
            self.instructions.truncate(emitted.position);
            self.last_instruction = mem::replace(&mut self.previous_instruction, None);
        }
    }

    fn set_last_instruction(&mut self, op_code: OpCode, position: usize) {
        self.previous_instruction = mem::replace(
            &mut self.last_instruction,
            Some(EmittedInstruction{op_code, position})
        )
    }
}