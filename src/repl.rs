use crate::{lexer::Lexer, parser::{Parser, ParserError}, evaluator, object::environment::Environment};
use std::{io::{self, Write}, rc::Rc, cell::RefCell};

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        let input = get_input();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            print_parser_errors(parser.errors);
            continue;
        }
        let env = Rc::new(RefCell::new(Environment::new()));
        let evaluation = evaluator::eval(&program, env);
        match evaluation {
            Ok(obj) => println!("{}", obj),
            Err(err) => println!("{}", err)
        }
    }
}

fn print_parser_errors(errors: Vec<ParserError>) {
    for error in errors {
        println!("{}", error);
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