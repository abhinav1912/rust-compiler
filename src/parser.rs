use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::Program;
use std::mem;
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token
}

impl Parser {
    pub fn new_parser(lexer: Lexer) -> Self {
        let mut parser = Parser{
            lexer: lexer,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program() -> Option<Program> {
        return None;
    }
}