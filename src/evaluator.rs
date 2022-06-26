use crate::{ast::{Program, Statement, Expression, Prefix}, object::{Object, EvalError, EvalResult}};

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
        _ => Ok(Object::Null)
    }
}

fn eval_prefix_expression(prefix: &Prefix, expression: &Expression) -> EvalResult {
    let obj = eval_expression(expression)?;
    match prefix {
        Prefix::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        Prefix::Minus => todo!(),
    }
}