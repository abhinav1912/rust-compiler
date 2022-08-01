use std::collections::HashMap;

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
        // TODO: Check scope
        let scope = SymbolScope::Global;
        let symbol = Symbol {
            scope,
            index: self.current.num_definitions
        };
        self.current.num_definitions += 1;
        self.current.define_symbol(name, symbol)
    }
}