use std::{collections::HashMap, mem};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Local
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Symbol {
    pub scope: SymbolScope,
    pub index: u16
}

#[derive(Default)]
struct SymbolLayer {
    store: HashMap<String, Symbol>,
    num_definitions: u16
}

#[derive(Default)]
pub struct SymbolTable {
    current: SymbolLayer,
    outers: Vec<SymbolLayer>
}

impl SymbolLayer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn define_symbol(&mut self, name: &str, symbol: Symbol) -> &Symbol {
        self.store.insert(name.to_string(), symbol);
        self.store.get(name).expect("inserted recently")
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let scope: SymbolScope;
        if self.outers.is_empty() {
            scope = SymbolScope::Global;
        } else {
            scope = SymbolScope::Local;
        }
        let symbol = Symbol {
            scope,
            index: self.current.num_definitions
        };
        self.current.num_definitions += 1;
        self.current.define_symbol(name, symbol)
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        let maybe_symbol: &Option<Symbol> = unsafe {
            mem::transmute(self.current.store.get(name))
        };
        if maybe_symbol.is_some() {
            return maybe_symbol.clone()
        } else {
            return None
        }
    }

    pub fn num_definitions(&self) -> u16 {
        self.current.num_definitions
    }

    pub fn push(&mut self) {
        let outer = mem::replace(&mut self.current, SymbolLayer::new());
        self.outers.push(outer);
    }
}