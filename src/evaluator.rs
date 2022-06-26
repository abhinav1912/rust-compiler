use crate::{ast::{Program, Statement, Expression, Prefix, Infix}, object::{Object, EvalError, EvalResult}};

pub fn eval(program: &Program) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement)?;
    }
    Ok(result)
}

fn eval_statement(statement: &Statement) -> EvalResult {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => Ok(Object::Null),
    }
}

fn eval_expression(expression: &Expression) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Prefix(prefix, expression) => eval_prefix_expression(prefix, expression),
        Expression::Infix(infix, left_exp, right_exp) => eval_infix_expression(infix, left_exp, right_exp),
        _ => Ok(Object::Null)
    }
}

fn eval_prefix_expression(prefix: &Prefix, expression: &Expression) -> EvalResult {
    let obj = eval_expression(expression)?;
    match prefix {
        Prefix::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        Prefix::Minus => match obj {
            Object::Integer(value) => Ok(Object::Integer(-value)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj))
        }
    }
}

fn eval_infix_expression(infix: &Infix, left_exp: &Expression, right_exp: &Expression) -> EvalResult {
    let left_obj = eval_expression(left_exp)?;
    let right_obj = eval_expression(right_exp)?;
    match (left_obj, right_obj) {
        (Object::Integer(left), Object::Integer(right)) => eval_integer_infix_expression(infix, left, right),
        _ => Err(EvalError::TypeMismatch(infix.clone(), left_obj, right_obj))
    }
}

fn eval_integer_infix_expression(infix: &Infix, left: i64, right: i64) -> EvalResult {

}

mod evaluator_tests {
    use crate::evaluator;
    use crate::{parser::Parser, object::EvalResult, lexer::Lexer};

    #[test]
    fn eval_boolean() {
        expect_values(vec![
            // Prefix
            ("!true", "false"),
            ("!!true", "true"),
            ("!false", "true"),
            ("!!false", "false"),
            ("!null", "true"),
            ("!!null", "false"),
            ("!0", "false"),
            ("!3", "false"),
            ("!!3", "true"),
        ]);
    }

    #[test]
    fn eval_integer() {
        expect_values(vec![
            // Prefix
            ("1", "1"),
            ("-123", "-123"),
            ("-(-123)", "123"),
        ]);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(obj.to_string(), expected.to_string(), "for `{}`", input);
                }
                Err(err) => {
                    panic!(
                        "expected `{}`, but got error=`{}` for `{}`",
                        expected, err, input
                    );
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new_parser(lexer);

        let program = parser.parse_program();
        evaluator::eval(&program)
    }
}