use std::{collections::HashMap, rc::Rc, cell::RefCell};
use crate::object::Object;

#[derive(Debug, PartialEq, Default)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => self
                .outer
                .as_ref()
                .and_then(|o| o.borrow().get(name).clone()),
        }
    }

    pub fn set(&mut self, name: &str, value: Object) {
        self.store.insert(name.to_string(), value);
    }

    pub fn extend(outer: Rc<RefCell<Self>>) -> Environment {
        Environment { store: HashMap::new(), outer: Some(outer) }
    }
}