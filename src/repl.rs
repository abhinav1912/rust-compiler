use crate::{lexer::Lexer, parser::{Parser, ParserError}, evaluator, object::environment::Environment, mode::Mode, compiler::{Compiler, symbol_table::SymbolTable}, vm::{Vm, self}};
use std::{io::{self, Write}, rc::Rc, cell::RefCell, env};

pub fn start(mode: Mode) {
    let username = env::var("LOGNAME").unwrap_or_else(|_| "anonymous".to_string());
    println!(
        "Hello {}! This is the repl in {}!",
        username, mode
    );
    println!("Feel free to type in commands");

    // For evaluator
    let env = Rc::new(RefCell::new(Environment::new()));

    // For compiler and VM
    // The compiler adds more constants and entries to the symbol table.
    let constants = Rc::new(RefCell::new(Vec::new()));
    let symbol_table = Rc::new(RefCell::new(SymbolTable::new_with_builtins()));
    // The VM adds more globals.
    let globals = Rc::new(RefCell::new(vm::new_globals()));

    loop {
        let input = ask_input(">> ");

        let mut parser = Parser::new_parser(Lexer::new(input));

        let program = parser.parse_program();
        if !parser.errors().is_empty() {
            println!("Woops! We ran into some monkey business here!");
            println!(" parser errors:");
            for error in parser.errors() {
                println!("\t{:?}", error);
            }
            continue;
        }

        match mode {
            Mode::Eval => match evaluator::eval(&program, Rc::clone(&env)) {
                Ok(obj) => {
                    println!("{}", obj);
                }
                Err(err) => {
                    println!("ERROR: {}", err);
                }
            },
            Mode::Compile => {
                let compiler =
                    Compiler::new_with_state(Rc::clone(&symbol_table), Rc::clone(&constants));
                let bytecode = match compiler.compile(&program) {
                    Ok(bytecode) => bytecode,
                    Err(err) => {
                        println!("Woops! Compilation failed: {}", err);
                        continue;
                    }
                };

                let vm = Vm::new_with_globals_store(bytecode, Rc::clone(&globals));
                match vm.run() {
                    Ok(result) => {
                        println!("{}", result);
                    }
                    Err(err) => {
                        println!("Woops! Executing bytecode failed: {}", err);
                    }
                }
            }
        }
    }
}

fn ask_input(prompt: &str) -> String {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut input = String::new();

    print!("{}", prompt);
    stdout.flush().expect("Failed to flush stdout");
    stdin
        .read_line(&mut input)
        .expect("Failed to read line from stdin");

    input
}
