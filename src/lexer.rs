use std::{iter::Peekable, str::Chars, mem};
use crate::token::{self, Token};

pub struct Lexer {
    input: String,
    position: usize,
    ch: char,
    chars: Peekable<Chars<'static>>
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars = unsafe { mem::transmute(input.chars().peekable()) };
        let mut lexer = Lexer {
            input,
            position: 0,
            ch: '\u{0}',
            chars,
        };
        lexer.read_char();
        lexer
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn next_token(&mut self) -> token::Token {
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
                    return return_token;
                } else if is_digit(self.ch) {
                    let integer_part = self.read_digit().to_string();
                    if self.ch == '.' && is_digit(self.peek_char()) {
                        self.read_char();
                        let decimal_part = self.read_digit();
                        return_token = Token::Float(format!("{}.{}", integer_part, decimal_part));
                    } else {
                        return_token = Token::Int(integer_part);
                    }
                    return return_token;
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
        // The first character needs to be a letter.
        if is_letter(self.ch) {
            self.read_char();
        }
        // The second character and after can be a letter or a digit.
        while is_letter(self.ch) || is_digit(self.ch) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_digit(&mut self) -> &str {
        let position: usize = self.position;
        while is_digit(self.ch) {
            self.read_char();
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

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        } 
    }

    fn read_char(&mut self) {
        // reads next character and advances position in the string
        self.position += if self.ch == '\u{0}' {
            0
        } else {
            self.ch.len_utf8()
        };
        self.ch = self.chars.next().unwrap_or('\u{0}');
    }

    fn peek_char(&mut self) -> char {
        self.chars.peek().cloned().unwrap_or('\u{0}')
    }
}

fn is_letter(ch: char) -> bool {
    ch == '_'
        || ch == '$'
        || ch.is_alphabetic()
        // emoji support
        || ('\u{203C}' <= ch && ch <= '\u{3299}')
        || ('\u{1F000}' <= ch && ch <= '\u{1FA95}')
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'
}