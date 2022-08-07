use std::rc::Rc;

use crate::{object::{Closure, Object}, code::Instructions};

pub struct Frame {
    closure: Closure,
    pub ip: usize,
    pub base_pointer: usize
}

impl Frame {
    pub fn new(closure: Closure, base_pointer: usize) -> Self {
        Frame { closure, ip: 0, base_pointer }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.closure.func.instructions
    }

    pub fn free_at(&self, index: usize) -> Rc<Object> {
        Rc::clone(&self.closure.free[index])
    }
}