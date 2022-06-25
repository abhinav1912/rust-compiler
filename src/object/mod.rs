pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null
}

#[derive(Debug)]
pub enum EvalError {

}