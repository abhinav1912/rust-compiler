use crate::ast::{Program, Statement, Expression, Prefix, Infix, BlockStatement};
use crate::object::{assert_argument_count, builtin, HashKey};
use crate::object::{Object, EvalError, EvalResult, environment::Environment};
use std::collections::HashMap;
use std::{rc::Rc, cell::RefCell};

pub fn eval(program: &Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(&env))?;
        if let Object::Return(value) = result {
            return Ok(*value)
        }
    }
    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> EvalResult {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Return(Some(expression)) => {
            let result = eval_expression(expression, Rc::clone(&env))?;
            Ok(Object::Return(Box::new(result)))
        },
        Statement::Return(None) => Ok(Object::Return(Box::new(Object::Null))),
        Statement::Let(name, exp) => {
            let result = eval_expression(exp, Rc::clone(&env))?;
            env.borrow_mut().set(name, result.clone());
            Ok(result)
        }
        _ => Ok(Object::Null),
    }
}

fn eval_expression(expression: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::StringLiteral(value) => Ok(Object::String(value.to_string())),
        Expression::Prefix(prefix, expression) => eval_prefix_expression(prefix, expression.as_ref(), env),
        Expression::Infix(infix, left_exp, right_exp) => eval_infix_expression(infix, left_exp.as_ref(), right_exp.as_ref(), env),
        Expression::If(condition, consequence, alternative) => eval_if_expression(condition.as_ref(), consequence, alternative.as_ref(), env),
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::FunctionLiteral(params, body) => Ok(Object::Function(params.to_vec(), body.clone(), env.clone())),
        Expression::Call(func, args) => {
            let function = eval_expression(func, Rc::clone(&env))?;
            let args = eval_expressions(args, env)?;
            apply_function(function, args)
        },
        Expression::Array(elements) => {
            let values = eval_expressions(elements, env)?;
            Ok(Object::Array(values))
        },
        Expression::Index(left, index) => eval_index_expression(left, index, env),
        Expression::Hash(pairs) => eval_hash_literal(pairs, env),
        _ => Ok(Object::Null)
    }
}

fn eval_prefix_expression(prefix: &Prefix, expression: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    let obj = eval_expression(expression, env)?;
    match prefix {
        Prefix::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        Prefix::Minus => match obj {
            Object::Integer(value) => Ok(Object::Integer(-value)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj))
        }
    }
}

fn eval_infix_expression(infix: &Infix, left_exp: &Expression, right_exp: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    let left_obj = eval_expression(left_exp, Rc::clone(&env))?;
    let right_obj = eval_expression(right_exp, env)?;
    match (left_obj, right_obj) {
        (Object::Integer(left), Object::Integer(right)) => eval_integer_infix_expression(infix, left, right),
        (Object::Boolean(left), Object::Boolean(right)) => eval_boolean_infix_expression(infix, left, right),
        (Object::String(left), Object::String(right)) => eval_string_infix_expression(infix, &left, &right),
        (left, right) => Err(EvalError::TypeMismatch(infix.clone(), left, right))
    }
}

fn eval_integer_infix_expression(infix: &Infix, left: i64, right: i64) -> EvalResult {
    match infix {
        Infix::Eq => Ok(Object::Boolean(left == right)),
        Infix::NotEq => Ok(Object::Boolean(left != right)),
        Infix::Lt => Ok(Object::Boolean(left < right)),
        Infix::Gt => Ok(Object::Boolean(left > right)),
        Infix::Plus => Ok(Object::Integer(left + right)),
        Infix::Minus => Ok(Object::Integer(left - right)),
        Infix::Asterisk => Ok(Object::Integer(left * right)),
        Infix::Slash => Ok(Object::Integer(left / right)),
    }
}

fn eval_boolean_infix_expression(infix: &Infix, left: bool, right: bool) -> EvalResult {
    match infix {
        Infix::Eq => Ok(Object::Boolean(left == right)),
        Infix::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::UnknownInfixOperator(
            infix.clone(),
            Object::Boolean(left),
            Object::Boolean(right)
        )),
    }
}

fn eval_string_infix_expression(infix: &Infix, left: &str, right: &str) -> EvalResult {
    match infix {
        Infix::Plus => Ok(Object::String([left, right].concat())),
       _ => Err(EvalError::UnknownInfixOperator(
            infix.clone(),
            Object::String(left.to_string()),
            Object::String(right.to_string())
        ))
    }
}

fn eval_if_expression(condition: &Expression, consequence: &BlockStatement, alternative: Option<&BlockStatement>, env: Rc<RefCell<Environment>>) -> EvalResult {
    let result = eval_expression(condition, Rc::clone(&env))?;
    if result.is_truthy() {
        eval_block_statement(consequence, env)
    } else {
        alternative
        .map(|a| eval_block_statement(a, env))
        .unwrap_or(Ok(Object::Null))
    }
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &block.statements {
        result = eval_statement(statement, Rc::clone(&env))?;
        if let Object::Return(_) = result {
            return Ok(result)
        }
    }
    Ok(result)
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> EvalResult {
    if let Some(object) = env.borrow().get(name) {
        return Ok(object.clone());
    }
    if let Some(obj) = builtin::lookup(name) {
        return Ok(obj)
    }
    Err(EvalError::IdentifierNotFound(name.to_string()))
}

fn eval_index_expression(left: &Expression, index: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    let left_evaluated = eval_expression(left, env.clone())?;
    let index_evaluated = eval_expression(index, env)?;
    match (left_evaluated, index_evaluated) {
        (Object::Array(array), Object::Integer(value)) => Ok(or_null(array.get(value as usize))),
        (l, i) => Err(EvalError::UnknownIndexOperator(l, i)),
    }
}

fn eval_expressions(expressions: &[Expression], env: Rc<RefCell<Environment>>) -> Result<Vec<Object>, EvalError> {
    let mut results = vec![];
    for expression in expressions {
        results.push(eval_expression(expression, Rc::clone(&env))?);
    }
    Ok(results)
}

fn eval_hash_literal(pairs: &[(Expression, Expression)], env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut map = HashMap::new();
    for (key, value) in pairs.iter() {
        let key_exp = eval_expression(key, env.clone())?;
        let val_exp = eval_expression(value, env.clone())?;
        let hashkey = HashKey::from_object(&key_exp)?;
        map.insert(hashkey, val_exp);
    }
    Ok(Object::Hash(map))
}

fn apply_function(function: Object, arguments: Vec<Object>) -> EvalResult {
    match function {
        Object::Function(params, body, env) => {
            assert_argument_count(params.len(), &arguments)?;
            let new_env = extend_function_env(params, arguments, env);
            let evaluated = eval_block_statement(&body, new_env)?;
            unwrap_return_value(evaluated)

        },
        Object::Builtin(func) => func(arguments),
        _ => Err(EvalError::NotCallable(function.clone()))
    }
}

fn extend_function_env(params: Vec<String>, arguments: Vec<Object>, env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
    let new_env = Rc::new(RefCell::new(Environment::extend(env)));
    for (i, param) in params.iter().enumerate() {
        let arg = or_null(arguments.get(i));
        new_env.borrow_mut().set(param, arg);
    }
    new_env
}

fn or_null(option: Option<&Object>) -> Object {
    option.cloned().unwrap_or(Object::Null)
}

fn unwrap_return_value(obj: Object) -> EvalResult {
    match obj {
        Object::Return(value) => Ok(*value),
        _ => Ok(obj),
    }
}

mod evaluator_tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::evaluator;
    use crate::object::environment::Environment;
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
            // Infix
            // boolean -> boolean
            ("true == true", "true"),
            ("false == true", "false"),
            ("true != true", "false"),
            ("true != false", "true"),
            // integer -> boolean
            ("1 == 2", "false"),
            ("2 == 2", "true"),
            ("1 != 2", "true"),
            ("2 != 2", "false"),
            ("1 > 2", "false"),
            ("1 < 2", "true"),
        ]);
    }

    #[test]
    fn eval_integer() {
        expect_values(vec![
            // Prefix
            ("1", "1"),
            ("-123", "-123"),
            ("-(-123)", "123"),
            ("1 + 2", "3"),
            ("1 - 2", "-1"),
            ("3 * -2", "-6"),
            ("4 / 2", "2"),
            ("-50 + 100 + -50", "0"),
            ("20 + 2 * -10", "0"),
            ("50 / 2 * 2 + 10", "60"),
            ("2 * (5 + 10)", "30"),
            ("3 * 3 * 3 + 10", "37"),
            ("3 * (3 * 3) + 10", "37"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
        ]);
    }

    #[test]
    fn eval_if() {
        expect_values(vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (null) { 1 } else { 2 }", "2"),
            ("if (2 > 1) { 3 } else { 4 }", "3"),
            ("if (2 < 1) { 3 } else { 4 }", "4"),
            ("if (1 < 2) { 3 }", "3"),
            ("if (1 > 2) { 3 }", "null"),
        ]);
    }

    #[test]
    fn eval_return() {
        expect_values(vec![
            ("return;", "null"),
            ("return 10;", "10"),
            ("1 + 2; return; 3 + 4", "null"),
            ("1 + 2; return 8; 3 + 4", "8"),
            ("3; return 8 * 2; 3 + 4", "16"),
            // Nested statements
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }",
                "10",
            ),
        ]);
    }

    #[test]
    fn function_application() {
        expect_values(vec![
            ("let identity = fn(x) { x; }; identity(5);", "5"),
            ("let identity = fn(x) { return x; }; identity(5);", "5"),
            ("let double = fn(x) { x * 2; }; double(5);", "10"),
            ("let add = fn(x, y) { x + y; }; add(10, 23);", "33"),
            (
                "let add = fn(x, y) { x + y; }; add(3 + 4, add(10, 23));",
                "40",
            ),
            ("fn(x) { x; }(5);", "5"),
            ("fn(x) { x; }(1); 5;", "5"),
        ]);
        expect_errors(vec![(
            "let add = fn(x, y) { x + y }; add(123); 3;",
            "wrong number of arguments: expected 2, given 1",
        )]);
    }

    #[test]
    fn array_map() {
        let map = "
            let map = fn(array, func) {
                let iter = fn(arr, acc) {
                    if (len(arr) == 0) {
                        acc
                    } else {
                        iter(rest(arr), push(acc, func(first(arr))))
                    }
                };
                iter(array, [])
            };
        ";
        expect_values(vec![
            (
                &with_def(map, "map([1, 2, 3], fn(x) { x * x })"),
                "[1, 4, 9]",
            ),
            (&with_def(map, "map([2], fn(x) { 3 * x })"), "[6]"),
            (&with_def(map, "map([], fn(x) { x * x })"), "[]"),
        ]);
    }

    #[test]
    fn array_reduce() {
        let reduce = "
            let reduce = fn(array, init, func) {
                let iter = fn (arr, acc) {
                    if (len(arr) == 0) {
                        acc
                    } else {
                        iter(rest(arr), func(acc, first(arr)))
                    }
                };
                iter(array, init);
            };
        ";
        expect_values(vec![(
            &with_def(reduce, "reduce([2, 3, 5], 1, fn(acc, x) { acc * x })"),
            "30",
        )]);
    }

    fn with_def(def: &str, code: &str) -> String {
        def.to_string() + code
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
        let env = Rc::new(RefCell::new(Environment::new()));
        evaluator::eval(&program, env)
    }

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected_message) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!("no error object returned. got=`{}` for `{}`", obj, input);
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected_message, "for `{}`", input);
                }
            }
        }
    }
}