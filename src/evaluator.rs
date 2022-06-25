use crate::{ast::{Program, Statement, Expression}, object::{Object, EvalError, EvalResult}};

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
        _ => Ok(Object::Null)
    }
}