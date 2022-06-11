mod token;
mod lexer;
mod repl;
mod parser;

fn main() {
    println!("Welcome to the compiler!");
    repl::start();
}
