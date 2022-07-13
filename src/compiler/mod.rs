use std::{cell::RefCell, rc::Rc};

use crate::{code::Instructions, object::Object};

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Object>>>,
    pub instructions: Rc<RefCell<Vec<Instructions>>>
}