use std::{convert::TryInto, char::from_digit, iter::Peekable, str::Chars, mem};
use crate::token::{self, Token};

pub struct Lexer {
    input: String,
    position: usize,
    ch: char,
    chars: Peekable<Chars<'static>>
}

impl Lexer {
    fn read_char(&mut self) {
        // reads next character and advances position in the string
        self.position += if self.ch == '\u{0}' {
            0
        } else {
            self.ch.len_utf8()
        };
        self.ch = self.chars.next().unwrap_or('\u{0}');
    }

    fn next_token(&mut self) -> token::Token {
        // parses next character and returns token
        self.skip_whitespace();
        let return_token: token::Token;
        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    return_token = Token::Eq;
                } else {
                    return_token = Token::Assign;
                }
            }
            ':' => {
                return_token = Token::Colon;
            }
            ';' => {
                return_token = Token::Semicolon;
            }
            '(' => {
                return_token = Token::Lparen;
            }
            ')' => {
                return_token = Token::Rparen;
            }
            ',' => {
                return_token = Token::Comma;
            }
            '+' => {
                return_token = Token::Plus;
            }
            '-' => {
                return_token = Token::Minus;
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    return_token = Token::NotEq;
                } else {
                    return_token = Token::Bang;
                }
            }
            '*' => {
                return_token = Token::Asterisk;
            }
            '/' => {
                return_token = Token::Slash;
            }
            '<' => {
                return_token = Token::Lt;
            }
            '>' => {
                return_token = Token::Gt;
            }
            '{' => {
                return_token = Token::Lbrace;
            }
            '}' => {
                return_token = Token::Rbrace;
            }
            '[' => {
                return_token = Token::Lbracket;
            }
            ']' => {
                return_token = Token::Rbracket;
            }
            '"' => {
                return_token = Token::String(self.read_string().to_string());
            }
            '\u{0}' => {
                return_token = Token::Eof;
            }
            _ => {
                if is_letter(self.ch) {
                    return_token = token::lookup_identifier(self.read_identifier());
                } else if is_digit(self.ch) {
                    return_token = Token::Int(self.read_digit().to_string());
                } else {
                    return_token = Token::Illegal;
                }
            }
        }
        self.read_char();
        return_token
    }

    fn read_identifier(&mut self) -> &str {
        // returns identifier for current block of characters
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char()
        }
        &self.input[position..self.position]
    }

    fn read_digit(&mut self) -> &str {
        let position: usize = self.position;
        while is_digit(self.ch) {
            self.read_char()
        }
        &self.input[position..self.position]
    }

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\u{0}' {
                break;
            }
        }
        &self.input[position..self.position]
    }

    fn peek_char(&mut self) -> char {
        self.chars.peek().cloned().unwrap_or('\u{0}')
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        } 
    }
}

pub fn new_lexer(input: String) -> Lexer {
    let chars = unsafe { mem::transmute(input.chars().peekable()) };
    let mut lexer = Lexer { 
        input,
        position: 0,
        ch: '\u{0}',
        chars
     };
    lexer.read_char();
    lexer 
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'
}