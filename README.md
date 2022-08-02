# Interpreter/Compiler in Rust

Create a compiler to parse, evaluate and execute a simple programming language (also custom defined).

## Feature set

- Create a simple language which supports common features such as
    - Variable bindings
    - Arithmetic expressions
    - Functions
    - Strings/Arrays
- Create an interpreter to parse the source code, including
    - Lexer
    - Parser
    - Abstract Syntax Tree
    - Evaluator for AST
- Bytecode compiler and virtual machine to execute bytecode

### Supported data-types:
- Booleans
- Numerals
    - Ints
    - Floats
- Strings
- Hash/Dictionary (dynamically typed)

### Example of supported programming statements/expressions:
```rust
let x = 10; // identifiers
let y = {"key1": "str", "key2": 5}; // dictionary, string

x + 20; // arithmetic, accessing variables
fn compare(a, b) {
    a == b
} // function declaration, comparison operators
compare(3.0, 3.1) // function invokation, floats
```

## Installation

1. Clone the repository
2. Install Rust using the steps mentioned [here](https://www.rust-lang.org/tools/install).
3. Run the project using the following possible commands:
```rust
// to run in Compile mode
cargo run compile
// OR, run in Evaluator mode
cargo run eval
```