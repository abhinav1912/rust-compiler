#[allow(dead_code)]
mod token {
    const ILLEGAL: &str = "ILLEGAL";
    const EOF: &str = "EOF";

    // Identifiers + literals
    const IDENT: &str = "IDENT";
    const INT: &str = "INT";

    // Operators
    const ASSIGN: &str = "=";
    const PLUS: &str = "+";

    // Delimiters
    const COMMA: &str = ",";
    const SEMICOLON: &str = ";";
    const LPAREN: &str = "(";
    const RPAREN: &str = ")";
    const LBRACE: &str = "{";
    const RBRACE: &str = "}";

    // Keywords
    const FUNCTION: &str = "FUNCTION";
    const LET: &str = "LET";

    type TokenType = String;

    struct Token {
        token_type: TokenType,
        literal: String
    }
}