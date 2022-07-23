use std::env;

use rust_compiler::{repl, mode::Mode};

fn main() {
    println!("Welcome to the compiler!");
    repl::start(eval_or_compile());
}

fn has_flag(flag: &str) -> bool {
    println!("{:?}", env::args());
    env::args().any(|arg| arg == flag)
}

fn eval_or_compile() -> Mode {
    // Mode::Compile
    if has_flag("compile") {
        Mode::Compile
    } else {
        Mode::Eval
    }
}