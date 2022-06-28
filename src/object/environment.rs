use std::collections::HashMap;
use crate::object::Object;

#[derive(Debug, PartialEq, Default)]
pub struct Environment {
    store: HashMap<String, Object>
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => None
        }
    }

    pub fn set(&mut self, name: &str, value: Object) {
        self.store.insert(name.to_string(), value);
    }
}