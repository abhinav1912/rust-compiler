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

    pub fn parse_program(&self) -> Program {
        let mut statements = vec![];
        Program { statements }
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
                Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
                Statement::Let("y".to_string(), Expression::IntegerLiteral(10)),
                Statement::Let("foo".to_string(), Expression::IntegerLiteral(155))
            ]
        )
    }
}