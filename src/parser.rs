use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token
}