use crate::{lexer::Lexer, token, token::Token};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        let input = get_input();
        let mut lexer = Lexer::new(input);
        let mut token: token::Token;
        loop {
            token = lexer.next_token();
            if token == Token::Eof {
                break;
            } else {
                println!("{}", token);
            }
        }
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