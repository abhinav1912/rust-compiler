mod token;
mod lexer;
mod repl;

fn main() {
    println!("Welcome to the compiler!");
    repl::start();
}
