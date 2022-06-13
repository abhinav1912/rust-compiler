use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Program, Statement, Expression};
use std::mem;

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdentifier(Token),
    ExpectedAssign(Token),
    ParsingNotImplemented
}
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>
}

impl Parser {
    pub fn new_parser(lexer: Lexer) -> Self {
        let mut parser = Parser{
            lexer: lexer,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![]
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];
        while self.curr_token != Token::Eof {
            let parse_result = self.parse_statement();
            match parse_result {
                Ok(statement) => statements.push(statement),
                Err(e) => self.errors.push(e)
            }
            self.next_token();
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.curr_token {
            _ => self.parse_let_statement(),
            _ => Err(ParserError::ParsingNotImplemented)
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let name;
        if let Token::Ident(identifier) = self.peek_token.clone() {
            self.next_token();
            name = identifier;
        } else {
            return Err(ParserError::ExpectedIdentifier(self.peek_token.clone()))
        }
        if !self.expect_peek(Token::Assign) {
            return Err(ParserError::ExpectedAssign(self.curr_token.clone()))
        }
        // skipping tokens till semicolon
        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }
        // TODO: parse value/expression
        let statement = Statement::Let(name, Expression::Identifier("".to_string()));
        Ok(statement)
    }

    fn curr_token_is(&self, token: Token) -> bool {
        return self.curr_token == token;
    }

    fn peek_token_is(&self, token: Token) -> bool {
        return self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            return true
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use std::iter::Enumerate;

    use crate::ast::{Statement, Expression};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    #[test]
    fn let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foo = 155;
        ";
        let mut lexer = Lexer::new(input.to_string().to_owned());
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();
        if program.statements.len() != 3 {
            panic!("program doesn't contain 3 statements")
        }
        assert_eq!(
            program.statements,
            vec![
                Statement::Let("x".to_string(), Expression::Identifier("".to_string())),
                Statement::Let("y".to_string(), Expression::Identifier("".to_string())),
                Statement::Let("foo".to_string(), Expression::Identifier("".to_string()))
            ]
        );
        check_parser_errors(parser);
    }

    fn check_parser_errors(parser: Parser) {
        if parser.errors.len() == 0 {
            return;
        }
        for message in parser.errors.iter() {
            println!("{}", message);
        }
        panic!("{} parser errors occured.", parser.errors.len());
    }
}