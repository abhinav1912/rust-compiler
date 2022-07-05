use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::{Program, Statement, Expression, Prefix, Infix, BlockStatement};
use std::{mem, fmt};

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdentifier(Token),
    ExpectedAssign(Token),
    ExpectedPrefixToken(Token),
    ExpectedInfixToken(Token),
    ExpectedIntegerToken(Token),
    ExpectedLParenToken(Token),
    ExpectedRParenToken(Token),
    ExpectedLBraceToken(Token),
    ExpectedRBraceToken(Token),
    ExpectedStringToken(Token),
    ExpectedLBracketToken(Token),
    ExpectedRBracketToken(Token),
    ExpectedSemicolon(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
    ParseInt(String),
    ParsingNotImplemented
}

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<ParserError>
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index
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
            _ => self.parse_expression_statement()
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
        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;

        self.next_token();
        let statement = Statement::Let(name, self.parse_expression(Precedence::Lowest)?);
        
        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.curr_token == Token::Semicolon {
            return Ok(Statement::Return(None));
        }
        let statement = Statement::Return(Some(self.parse_expression(Precedence::Lowest)?));
        if self.peek_token_is(Token::Semicolon) {
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
        .prefix_parse_fn()
        .ok_or_else(|| ParserError::ExpectedPrefixToken(self.peek_token.clone()))?;
        let mut left_exp = prefix(self)?;
        while !self.peek_token_is(Token::Semicolon) && precedence < self.infix_token(&self.peek_token).0 {
            if let Some(infix) = self.infix_parse_fn() {
                self.next_token();
                left_exp = infix(self, left_exp)?;
            } else {
                return Ok(left_exp);
            }
        }
        Ok(left_exp)
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        match self.curr_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer),
            Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
            Token::True | Token::False => Some(Parser::parse_boolean),
            Token::Lparen => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_literal),
            Token::String(_) => Some(Parser::parse_string_literal),
            Token::Lbracket => Some(Parser::parse_array_literal),
            Token::Lbrace => Some(Parser::parse_hash_literal),
            _ => None
        }
    }

    fn infix_parse_fn(&self) -> Option<InfixParseFn> {
        match &self.peek_token {
            Token::Plus => Some(Parser::parse_infix_expression),
            Token::Minus => Some(Parser::parse_infix_expression),
            Token::Asterisk => Some(Parser::parse_infix_expression),
            Token::Slash => Some(Parser::parse_infix_expression),
            Token::Eq => Some(Parser::parse_infix_expression),
            Token::NotEq => Some(Parser::parse_infix_expression),
            Token::Lt => Some(Parser::parse_infix_expression),
            Token::Gt => Some(Parser::parse_infix_expression),
            Token::Lparen => Some(Parser::parse_call_expression),
            Token::Lbracket => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn parse_integer(&mut self) -> Result<Expression> {
        if let Token::Int(int) = &self.curr_token {
            match int.parse() {
                Ok(value) => Ok(Expression::IntegerLiteral(value)),
                Err(_) => Err(ParserError::ParseInt(int.to_string()))
            }
        } else {
            Err(ParserError::ExpectedIntegerToken(self.peek_token.clone()))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        // prefix token is the current token
        let p = self.prefix_token(&self.curr_token)?;
        self.next_token();
        // parse expression on the right
        let expression = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(p, Box::new(expression)))

    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let (precedence, infix) = self.infix_token(&self.curr_token);
        let i = infix.ok_or_else(|| ParserError::ExpectedInfixToken(self.peek_token.clone()))?;
        self.next_token();
        let right = self.parse_expression(precedence)?;
        return Ok(Expression::Infix(i, Box::new(left), Box::new(right)));
    }

    fn parse_boolean(&mut self) -> Result<Expression> {
        return Ok(Expression::Boolean(self.curr_token_is(Token::True)))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::Rparen, ParserError::ExpectedRParenToken)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expect_peek(Token::Lparen, ParserError::ExpectedLParenToken)?;

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        
        self.expect_peek(Token::Rparen, ParserError::ExpectedRParenToken)?;

        self.expect_peek(Token::Lbrace, ParserError::ExpectedLBraceToken)?;

        let consequence = self.parse_block_statement()?;
        let mut alternative: Option<BlockStatement> = None;
        if self.peek_token_is(Token::Else) {
            self.next_token();

            self.expect_peek(Token::Lbrace, ParserError::ExpectedLBraceToken)?;

            alternative = Some(self.parse_block_statement()?);
        }
        return Ok(Expression::If(Box::new(condition), consequence, alternative));
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut statements: Vec<Statement> = vec![];
        self.next_token();

        while !self.curr_token_is(Token::Rbrace) && !self.curr_token_is(Token::Eof) {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        let block_statement = BlockStatement{ statements };
        Ok(block_statement)
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        self.expect_peek(Token::Lparen, ParserError::ExpectedLParenToken)?;

        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::Lbrace, ParserError::ExpectedLBraceToken)?;
        let body = self.parse_block_statement()?;
        return Ok(Expression::FunctionLiteral(parameters, body))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let arguments = self.parse_expressions(Token::Rparen, ParserError::ExpectedRParenToken)?;
        Ok(Expression::Call(Box::new(function), arguments))
    }

    fn parse_expressions(&mut self, closing_token: Token, expected: fn(Token) -> ParserError) -> Result<Vec<Expression>> {
        let mut expressions = vec![];

        if self.peek_token == closing_token {
            self.next_token();
            return Ok(expressions);
        }

        self.next_token();
        expressions.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token == Token::Comma {
            self.next_token();
            // cur_token: ,
            self.next_token();
            // cur_token: the first token of the current expression
            expressions.push(self.parse_expression(Precedence::Lowest)?);
            // cur_token: the last token of the current expression
        }
        self.expect_peek(closing_token, expected)?;
        // cur_token: closing_token

        Ok(expressions)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>> {
        let mut identifiers: Vec<String> = vec![];
        if self.peek_token_is(Token::Rparen) {
            self.next_token();
            return Ok(identifiers)
        }

        self.next_token();
        // curr_token: first parameter
        identifiers.push(self.parse_identifier_string()?);

        while self.peek_token == Token::Comma {
            // curr_token: previous param
            self.next_token();
            // curr_token: comma
            self.next_token();
            // curr_token: current param
            identifiers.push(self.parse_identifier_string()?);
        }
        self.expect_peek(Token::Rparen, ParserError::ExpectedRParenToken)?;
        Ok(identifiers)
    }

    fn prefix_token(&self, token: &Token) -> Result<Prefix> {
        match token {
            Token::Bang => Ok(Prefix::Bang),
            Token::Minus => Ok(Prefix::Minus),
            _ => Err(ParserError::ExpectedPrefixToken(token.clone()))
        }
    }

    fn infix_token(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::Eq => (Precedence::Equals, Some(Infix::Eq)),
            Token::NotEq => (Precedence::Equals, Some(Infix::NotEq)),
            Token::Lt => (Precedence::LessGreater, Some(Infix::Lt)),
            Token::Gt => (Precedence::LessGreater, Some(Infix::Gt)),
            Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
            Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
            Token::Slash => (Precedence::Product, Some(Infix::Slash)),
            Token::Asterisk => (Precedence::Product, Some(Infix::Asterisk)),
            Token::Lparen => (Precedence::Call, None),
            Token::Lbracket => (Precedence::Index, None),
            _ => (Precedence::Lowest, None),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        self.parse_identifier_string().map(Expression::Identifier)
    }

    fn parse_identifier_string(&self) -> Result<String> {
        if let Token::Ident(ident) = &self.curr_token {
            Ok(ident.to_string())
        } else {
            Err(ParserError::ExpectedIdentifier(self.peek_token.clone()))
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expression> {
        if let Token::String(value) = &self.curr_token {
            return Ok(Expression::StringLiteral(value.to_string()))
        }
        Err(ParserError::ExpectedStringToken(self.curr_token.clone()))
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        let exps = self.parse_expressions(Token::Rbracket, ParserError::ExpectedRBracketToken)?;
        Ok(Expression::Array(exps))
    }

    fn parse_index_expression(&mut self, array: Expression) -> Result<Expression> {
        // curr_token: [
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(Token::Rbracket, ParserError::ExpectedRBracketToken)?;
        let exp = Expression::Index(Box::new(array), Box::new(index));
        Ok(exp)
    }

    fn parse_hash_literal(&mut self) -> Result<Expression> {
        let mut pairs = vec![];
        while !self.peek_token_is(Token::Rbrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek(Token::Colon, ParserError::ExpectedColon)?;
            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));
            if self.peek_token != Token::Rbrace {
                self.expect_peek(Token::Comma, ParserError::ExpectedComma)?;
            }
        }
        self.expect_peek(Token::Rbrace, ParserError::ExpectedRBraceToken)?;

        Ok(Expression::Hash(pairs))
    }

    fn curr_token_is(&self, token: Token) -> bool {
        return self.curr_token == token;
    }

    fn peek_token_is(&self, token: Token) -> bool {
        return self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if !self.peek_token_is(token) {
            return Err(expected(self.peek_token.clone()))
        }
        self.next_token();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Statement, Expression, Prefix, Infix};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    #[test]
    fn let_statement() {
        let input = "
        let x = 5;
        let y = 10;
        let foo = x + y;
        ";
        let mut lexer = Lexer::new(input.to_string().to_owned());
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();

        check_parser_errors(parser);
        assert_eq!(
            program.statements,
            vec![
                Statement::Let("x".to_string(), Expression::IntegerLiteral(5)),
                Statement::Let("y".to_string(), Expression::IntegerLiteral(10)),
                Statement::Let(
                    "foo".to_string(),
                    Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier("x".to_string())),
                        Box::new(Expression::Identifier("y".to_string()))
                    )
                )
            ]
        );
    }

    #[test]
    fn return_statement() {
        let input = "
        return;
        return 5;
        return 10;
        return 999;
        ";
        let lexer = Lexer::new(input.to_string().to_owned());
        let mut parser = Parser::new_parser(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        assert_eq!(
            program.statements,
            vec![
                Statement::Return(None),
                Statement::Return(Some(Expression::IntegerLiteral(5))),
                Statement::Return(Some(Expression::IntegerLiteral(10))),
                Statement::Return(Some(Expression::IntegerLiteral(999))),
            ]
        );
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new_parser(lexer);

        let program = parser.parse_program();
        check_parser_errors(parser);

        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::Identifier(
                "foobar".to_string()
            )),]
        );
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new_parser(lexer);

        let program = parser.parse_program();
        check_parser_errors(parser);

        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::IntegerLiteral(5)),]
        );
    }

    #[test]
    fn prefix_expression() {
        let tests = vec![
            ("!20;", Prefix::Bang, Expression::IntegerLiteral(20)),
            ("-3;", Prefix::Minus, Expression::IntegerLiteral(3)),
        ];
        for (input, operator, value) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new_parser(lexer);

            let program = parser.parse_program();
            check_parser_errors(parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Prefix(
                    operator,
                    Box::new(value)
                ))]
            );
        }
    }

    #[test]
    fn infix_expression_integer() {
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gt, 5),
            ("5 < 5;", 5, Infix::Lt, 5),
            ("5 == 5;", 5, Infix::Eq, 5),
            ("5 != 5;", 5, Infix::NotEq, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new_parser(lexer);

            let program = parser.parse_program();
            check_parser_errors(parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::IntegerLiteral(left)),
                    Box::new(Expression::IntegerLiteral(right))
                ))]
            );
        }
    }

    #[test]
    fn infix_expression_boolean() {
        let tests = vec![
            ("true == true", true, Infix::Eq, true),
            ("true != false", true, Infix::NotEq, false),
            ("false == false", false, Infix::Eq, false),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new_parser(lexer);

            let program = parser.parse_program();
            check_parser_errors(parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::Boolean(left)),
                    Box::new(Expression::Boolean(right))
                ))]
            );
        }
    }

    #[test]
    fn hash() {
        test_parsing(vec![
            ("{}", "{};"),
            ("{1: 2, 2: 3}", "{1: 2, 2: 3};"),
            ("{true: 3}", "{true: 3};"),
            (
                r#"{"one": 1, "two": 2, "three": 3}"#,
                r#"{"one": 1, "two": 2, "three": 3};"#,
            ),
            // Duplicated entries
            (
                r#"{"one": 1, "one": 1, "two": 2}"#,
                r#"{"one": 1, "one": 1, "two": 2};"#,
            ),
        ]);
    }

    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new_parser(lexer);

            let program = parser.parse_program();
            check_parser_errors(parser);

            assert_eq!(program.to_string(), expected);
        }
    }

    fn check_parser_errors(parser: Parser) {
        if parser.errors.len() == 0 {
            return;
        }
        for message in parser.errors.iter() {
            println!("{:?}", message);
        }
        panic!("{} parser errors occured.", parser.errors.len());
    }
}