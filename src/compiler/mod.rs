pub mod symbol_table;

use std::{cell::RefCell, rc::Rc, fmt, mem};
use crate::{ast::{Program, Infix, Statement, Expression, BlockStatement, Prefix}, code::{Instructions, Constant, OpCode, self, CompiledFunction, ByteCode}, object::Object};
use self::symbol_table::{SymbolTable, SymbolScope, Symbol};

const TENTATIVE_JUMP_POS: u16 = 9999;

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Constant>>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new_with_builtins()));
        let constants = Rc::new(RefCell::new(vec![]));
        Compiler::new_with_state(symbol_table, constants)
    }
}

impl Compiler {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_state(
        symbol_table: Rc<RefCell<SymbolTable>>,
        constants: Rc<RefCell<Vec<Constant>>>,
    ) -> Self {
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
            Statement::Expression(exp) => {
                self.compile_expression(exp)?;
                self.emit(OpCode::Pop);
            }
            Statement::Let(name, exp) => {
                // Define the symbol first so that recursive functions can reference themselves in
                // their function bodies.
                let symbol = *self.symbol_table.borrow_mut().define(name);

                self.compile_expression(exp)?;

                match symbol.scope {
                    SymbolScope::Global => {
                        self.emit_with_operands(OpCode::SetGlobal, OpCode::u16(symbol.index));
                    }
                    SymbolScope::Local => {
                        self.emit_with_operands(OpCode::SetLocal, vec![symbol.index as u8]);
                    }
                    SymbolScope::Free => {
                        panic!("free cannot be defined by let statement");
                    }
                    SymbolScope::Builtin => {
                        panic!("builtin cannot be defined by let statement");
                    }
                }
            }
            Statement::Return(None) => {
                self.emit(OpCode::Return);
            }
            Statement::Return(Some(value)) => {
                self.compile_expression(value)?;
                self.emit(OpCode::ReturnValue);
            }
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
            Expression::Prefix(prefix, right) => {
                self.compile_expression(right)?;

                match prefix {
                    Prefix::Bang => {
                        self.emit(OpCode::Bang);
                    }
                    Prefix::Minus => {
                        self.emit(OpCode::Minus);
                    }
                }
            }
            Expression::IntegerLiteral(value) => {
                let constant = Constant::Integer(*value);
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            Expression::FloatLiteral(value) => {
                let constant = Constant::Float(*value);
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            Expression::Boolean(true) => {
                self.emit(OpCode::True);
            }
            Expression::Boolean(false) => {
                self.emit(OpCode::False);
            }
            Expression::StringLiteral(value) => {
                let constant = Constant::String(value.clone());
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            // if (condition) { consequence }
            //
            // should emit:
            //
            // 1. condition
            // 2. jump to 5 if not truthy
            // 3. consequence (without the last pop)
            // 4. jump to 6
            // 5. push null
            // 6. pop
            //
            // --
            //
            // if (condition) { consequence } else { alternative }
            //
            // should emit:
            //
            // 1. condition
            // 2. jump to 5 if not truthy
            // 3. consequence (without the last pop)
            // 4. jump to 6
            // 5. alternative (without the last pop)
            // 6. pop
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;
                let jump_not_truthy_pos = self
                    .emit_with_operands(OpCode::JumpIfNotTruthy, OpCode::u16(TENTATIVE_JUMP_POS));

                self.compile_block_statement(consequence)?;
                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_pop();
                }

                let jump_pos =
                    self.emit_with_operands(OpCode::Jump, OpCode::u16(TENTATIVE_JUMP_POS));

                self.replace_instruction(
                    jump_not_truthy_pos,
                    code::make_u16(
                        OpCode::JumpIfNotTruthy,
                        self.current_instructions().len() as u16,
                    ),
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
                    code::make_u16(OpCode::Jump, self.current_instructions().len() as u16),
                );
            }
            Expression::Identifier(name) => {
                let symbol = {
                    match self.symbol_table.borrow_mut().resolve(name) {
                        Some(symbol) => symbol,
                        None => return Err(CompileError::UndefinedVariable(name.to_string())),
                    }
                };
                self.load_symbol(symbol);
            }
            Expression::Array(exps) => {
                for exp in exps {
                    self.compile_expression(exp)?;
                }
                self.emit_with_operands(OpCode::Array, OpCode::u16(exps.len() as u16));
            }
            Expression::Hash(pairs) => {
                for (key, value) in pairs {
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }
                self.emit_with_operands(OpCode::Hash, OpCode::u16(pairs.len() as u16));
            }
            Expression::Index(left, index) => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;
                self.emit(OpCode::Index);
            }
            Expression::FunctionLiteral(params, body) => {
                let num_params = params.len();
                if num_params > 0xff {
                    return Err(CompileError::TooManyParams);
                }

                self.enter_scope();

                for param in params {
                    self.symbol_table.borrow_mut().define(param);
                }

                self.compile_block_statement(body)?;
                // Take care of implicit return like `fn() { 5 }`
                if self.last_instruction_is(OpCode::Pop) {
                    self.replace_last_pop_with_return();
                }
                // Take care of empty body like `fn() { }`
                if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.emit(OpCode::Return);
                }

                let num_locals = self.symbol_table.borrow().num_definitions();
                if num_locals > 0xff {
                    return Err(CompileError::TooManyLocals);
                }
                let (instructions, free_symbols) = self.leave_scope();

                let num_frees = free_symbols.len();
                if num_frees > 0xff {
                    return Err(CompileError::TooManyFrees);
                }

                // Load free variables before the OpCode::Closure so that the VM can build a closure
                // with free variables.
                for free in free_symbols {
                    self.load_symbol(free);
                }

                let compiled_function = Constant::CompiledFunction(CompiledFunction {
                    instructions,
                    num_locals: num_locals as u8,
                    num_parameters: num_params as u8,
                });
                let const_index = self.add_constant(compiled_function)?;
                self.emit_with_operands(
                    OpCode::Closure,
                    OpCode::u16_u8(const_index, num_frees as u8),
                );
            }
            Expression::Call(func, args) => {
                self.compile_expression(func)?;
                for arg in args {
                    self.compile_expression(arg)?;
                }
                self.emit_with_operands(OpCode::Call, vec![args.len() as u8]);
            }
        }
        Ok(())
    }

    fn compile_block_statement(
        &mut self,
        block_statement: &BlockStatement,
    ) -> Result<(), CompileError> {
        for statement in &block_statement.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();
        self.scopes.push(scope);
        self.scope_index += 1;

        self.symbol_table.borrow_mut().push();
    }

    fn leave_scope(&mut self) -> (Instructions, Vec<Symbol>) {
        let scope = self.scopes.pop().expect("no scope to leave from");
        self.scope_index -= 1;

        let free_symbols = self.symbol_table.borrow_mut().pop();

        (scope.instructions, free_symbols)
    }

    fn add_constant(&mut self, constant: Constant) -> Result<u16, CompileError> {
        let constant_index = self.constants.borrow().len();
        if constant_index >= 0xffff {
            return Err(CompileError::TooManyConstants);
        }
        self.constants.borrow_mut().push(constant);
        Ok(constant_index as u16)
    }

    fn emit(&mut self, op_code: OpCode) -> usize {
        // TODO: Isn't it slow to create `vec![]` each time?
        self.emit_with_operands(op_code, vec![])
    }

    fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        self.scopes[self.scope_index].emit_with_operands(op_code, operands)
    }

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
    fn replace_instruction(&mut self, pos: usize, instruction: Instructions) {
        self.scopes[self.scope_index].replace_instruction(pos, instruction)
    }

    fn last_instruction_is(&self, op_code: OpCode) -> bool {
        self.scopes[self.scope_index].last_instruction_is(op_code)
    }

    fn remove_last_pop(&mut self) {
        self.scopes[self.scope_index].remove_last_pop()
    }

    fn replace_last_pop_with_return(&mut self) {
        self.scopes[self.scope_index].replace_last_pop_with_return()
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            SymbolScope::Builtin => {
                self.emit_with_operands(OpCode::GetBuiltin, vec![symbol.index as u8]);
            }
            SymbolScope::Global => {
                self.emit_with_operands(OpCode::GetGlobal, OpCode::u16(symbol.index));
            }
            SymbolScope::Free => {
                self.emit_with_operands(OpCode::GetFree, vec![symbol.index as u8]);
            }
            SymbolScope::Local => {
                self.emit_with_operands(OpCode::GetLocal, vec![symbol.index as u8]);
            }
        }
    }

    fn bytecode(&self) -> ByteCode {
        let scope = &self.scopes[self.scope_index];
        // TODO: Can't this be done without cloning? Compiler's ownership moves to Bytecode anyway...
        ByteCode::new(scope.instructions.clone(), self.constants.borrow().clone())
    }
}

pub struct EmittedInstruction {
    pub op_code: OpCode,
    pub position: usize,
}

#[derive(Default)]
pub struct CompilationScope {
    pub instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
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
        pos
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
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

    fn set_last_instruction(&mut self, op_code: OpCode, position: usize) {
        self.previous_instruction = mem::replace(
            &mut self.last_instruction,
            Some(EmittedInstruction { op_code, position }),
        );
    }

    pub fn last_instruction_is(&self, op_code: OpCode) -> bool {
        match &self.last_instruction {
            Some(emitted) => emitted.op_code == op_code,
            None => false,
        }
    }

    pub fn remove_last_pop(&mut self) {
        if let Some(emitted) = &self.last_instruction {
            self.instructions.truncate(emitted.position);
            self.last_instruction = mem::replace(&mut self.previous_instruction, None);
        }
    }

    pub fn replace_last_pop_with_return(&mut self) {
        if let Some(last) = &self.last_instruction {
            let position = last.position;
            self.replace_instruction(position, code::make(OpCode::ReturnValue));
            self.last_instruction = Some(EmittedInstruction {
                position,
                op_code: OpCode::ReturnValue,
            });
        }
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
