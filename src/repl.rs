use crate::{lexer::Lexer, parser::{Parser, ParserError}, evaluator, object::environment::Environment, mode::Mode, compiler::{Compiler, symbol_table::SymbolTable}, vm::{Vm, self}};
use std::{io::{self, Write}, rc::Rc, cell::RefCell};

const PROMPT: &str = ">> ";

pub fn start(mode: Mode) {
    let env = Rc::new(RefCell::new(Environment::new()));
    let constants = Rc::new(RefCell::new(Vec::new()));
    let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
    loop {
        let input = get_input();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            print_parser_errors(parser.errors);
            continue;
        }
        match mode {
            Mode::Eval => match evaluator::eval(&program, Rc::clone(&env)) {
                Ok(obj) => println!("{}", obj),
                Err(err) => println!("{}", err)
            },
            Mode::Compile => {
                let compiler = Compiler::new_with_state(Rc::clone(&symbol_table), Rc::clone(&constants));
                let bytecode = match compiler.compile(&program) {
                    Ok(bytecode) => bytecode,
                    Err(err) => {
                        println!("Compilation failed: {}", err);
                        continue;
                    }
                };
                let vm = Vm::new(bytecode);
                match vm.run() {
                    Ok(result) => println!("{}", result),
                    Err(err) => println!("{}", err)
                }
            }
        }
    }
}

fn print_parser_errors(errors: Vec<ParserError>) {
    for error in errors {
        println!("\t{:?}", error);
    }
}

fn get_input() -> String {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut input = String::new();
    print!("{}", PROMPT);
    stdout.flush().expect("Failed to flush");
    stdin.read_line(&mut input).expect("Failed to read line");
    input
}