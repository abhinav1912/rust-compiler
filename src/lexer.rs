mod lexer {
    use std::{convert::TryInto, char::from_digit};

    use crate::token::*;

    #[derive(Default)]
    struct Lexer {
        input: String,
        position: u32,
        read_position: u32,
        ch: char
    }

    impl Lexer {
        fn read_char(&mut self) {
            // reads next character and advances position in the string
            if self.read_position >= self.input.len().try_into().unwrap() {
                self.ch = from_digit(0, 10).unwrap();
            } else {
                self.ch = self.input.chars().nth(self.read_position as usize).unwrap();
            }
            self.position = self.read_position;
            self.read_position += 1;
        }

        fn next_token(&mut self) -> token::Token {
            let return_token: token::Token;
            match self.ch {
                '=' => return_token = new_token(token::ASSIGN.to_string(), self.ch),
                ';' => return_token = new_token(token::SEMICOLON.to_string(), self.ch),
                '(' => return_token = new_token(token::LPAREN.to_string(), self.ch),
                ')' => return_token = new_token(token::RPAREN.to_string(), self.ch),
                ',' => return_token = new_token(token::COMMA.to_string(), self.ch),
                '+' => return_token = new_token(token::PLUS.to_string(), self.ch),
                '{' => return_token = new_token(token::LBRACE.to_string(), self.ch),
                '}' => return_token = new_token(token::RBRACE.to_string(), self.ch),
                _ => return_token = new_token(token::EOF.to_string(), '\u{0}')
            }
            self.read_char();
            return return_token;
        }
    }

    fn new_lexer(input: String) -> Lexer {
        let mut lexer = Lexer { input: (input), ..Default::default() };
        lexer.read_char();
        return lexer; 
    }

    fn new_token(token_type: token::TokenType, ch: char) -> token::Token {
        return token::Token{ token_type, literal: ch.to_string() }
    }
}