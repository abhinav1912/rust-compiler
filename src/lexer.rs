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

#[cfg(test)]
mod tests {
    use crate::lexer::{self, Lexer};
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            0
            12.345
            0.12
            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
            foo
            Bar_123
            [1, 2, 3];
            {"foo": "bar"}
            "日本語"
            識別子
            "🐒"
            let 🙈🙉🙊 = "見ざる聞かざる言わざる"
        "#;

        let tests = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("0".to_string()),
            Token::Float("12.345".to_string()),
            Token::Float("0.12".to_string()),
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Ident("foo".to_string()),
            Token::Ident("Bar_123".to_string()),
            Token::Lbracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::Comma,
            Token::Int("3".to_string()),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::Rbrace,
            Token::String("日本語".to_string()),
            Token::Ident("識別子".to_string()),
            Token::String("🐒".to_string()),
            Token::Let,
            Token::Ident("🙈🙉🙊".to_string()),
            Token::Assign,
            Token::String("見ざる聞かざる言わざる".to_string()),
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input.to_owned());

        for (i, expected_token) in tests.iter().enumerate() {
            let token = lexer.next_token();

            assert_eq!(&token, expected_token, "tests[{}] - token", i);
        }
    }
}