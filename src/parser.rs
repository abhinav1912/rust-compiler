use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Program, Statement, Expression};
use std::{mem, fmt};

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdentifier(Token),
    ExpectedAssign(Token),
    ExpectedPrefixToken(Token),
    ParsingNotImplemented
}
pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

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
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
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

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let statement = Statement::Return(None);
        self.next_token();
        while self.curr_token != Token::Semicolon {
            self.next_token();
        }
        Ok(statement)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(Precedence::Lowest);
        if self.peek_token == Token::Semicolon {
            self.next_token()
        }
        expression.map(Statement::Expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let prefix = self
        .parse_prefix_fn()
        .ok_or_else(|| ParserError::ExpectedPrefixToken(self.curr_token.clone()))?;
        let left_exp = prefix(self);
        left_exp
    }

    fn parse_prefix_fn(&self) -> Option<PrefixParseFn> {
        match self.curr_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            _ => None
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        self.parse_identifier_string().map(Expression::Identifier)
    }

    fn parse_identifier_string(&self) -> Result<String> {
        if let Token::Ident(ident) = &self.curr_token {
            Ok(ident.to_string())
        } else {
            Err(ParserError::ExpectedIdentifier(self.curr_token.clone()))
        }
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

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::ExpectedIdentifier(token) => write!(f, "expected identifier, got {}", token),
            ParserError::ExpectedAssign(token) => write!(f, "expected =, got {}", token),
            ParserError::ExpectedPrefixToken(token) => write!(f, "expected prefix, got {}", token),
            ParserError::ParsingNotImplemented => write!(f, "parsing not implemented for token"),
        }
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
        let x 5;
        let = 10;
        let 155;
        ";
        let mut lexer = Lexer::new(input.to_string().to_owned());
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();

        check_parser_errors(parser);
    }

    #[test]
    fn return_statement() {
        let input = "
        return 5;
        return 10;
        return 999;
        ";
        let lexer = Lexer::new(input.to_string().to_owned());
        let mut parser = Parser::new_parser(lexer);
        parser.parse_program();
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