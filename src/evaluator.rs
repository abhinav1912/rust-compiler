use crate::{ast::{Program, Statement, Expression}, object::{Object, EvalError, EvalResult}};

pub fn eval(program: &Program) -> Object {
    let mut result = Object::Null;
    for statement in &program.statements {
        // result = 
    }
    result
}

fn eval_statement(statement: &Statement) -> EvalResult {
    return Ok(Object::Null);
}

fn eval_expression(expression: Expression) -> EvalResult {
    return Ok(Object::Null);
}