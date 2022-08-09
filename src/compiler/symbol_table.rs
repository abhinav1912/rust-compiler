use std::{collections::HashMap, mem};

use crate::object::builtin;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolScope {
    Global,
    Local,
    Free,
    Builtin,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Symbol {
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Default)]
struct SymbolLayer {
    store: HashMap<String, Symbol>,
    num_definitions: u16,
    pub free_symbols: Vec<Symbol>,
}

impl SymbolLayer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn define_free(&mut self, name: &str, original: Symbol) -> Symbol {
        let symbol = Symbol {
            index: self.free_symbols.len() as u16,
            scope: SymbolScope::Free,
        };

        self.free_symbols.push(original);
        *self.define_symbol(name, symbol)
    }

    pub fn define_symbol(&mut self, name: &str, symbol: Symbol) -> &Symbol {
        self.store.insert(name.to_string(), symbol);
        self.store.get(name).expect("inserted just now")
    }
}

#[derive(Default)]
pub struct SymbolTable {
    current: SymbolLayer,
    outers: Vec<SymbolLayer>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_builtins() -> Self {
        let mut symbol_table = SymbolTable::new();
        for (i, b) in builtin::BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i as u16, b.name);
        }
        symbol_table
    }

    pub fn push(&mut self) {
        let outer = mem::replace(&mut self.current, SymbolLayer::new());
        self.outers.push(outer);
    }

    pub fn pop(&mut self) -> Vec<Symbol> {
        match self.outers.pop() {
            Some(outer) => {
                let popped = mem::replace(&mut self.current, outer);
                popped.free_symbols
            }
            None => vec![],
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let scope = if self.outers.is_empty() {
            SymbolScope::Global
        } else {
            SymbolScope::Local
        };
        let symbol = Symbol {
            index: self.current.num_definitions,
            scope,
        };
        self.current.num_definitions += 1;

        self.current.define_symbol(name, symbol)
    }

    pub fn define_builtin(&mut self, index: u16, name: &str) -> &Symbol {
        if !self.outers.is_empty() {
            panic!("builtin can be defined only on top-level scope");
        }

        let symbol = Symbol {
            index,
            scope: SymbolScope::Builtin,
        };

        self.current.define_symbol(name, symbol)
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        {
            // Silence the borrow checker.
            // https://users.rust-lang.org/t/solved-borrow-doesnt-drop-returning-this-value-requires-that/24182
            let maybe_symbol: Option<&Symbol> =
                unsafe { mem::transmute(self.current.store.get(name)) };
            if maybe_symbol.is_some() {
                return maybe_symbol.copied();
            }
        }

        let num_outers = self.outers.len();
        // Try from the 2nd innermost store to the outermost one.
        for (i, outer) in self.outers.iter().rev().enumerate() {
            if let Some(original) = outer.store.get(name) {
                return match original.scope {
                    SymbolScope::Global | SymbolScope::Builtin => Some(*original),
                    SymbolScope::Local | SymbolScope::Free => {
                        // If the symbol doesn't exist in the current scope but exists in an outer
                        // scope, define it as a free scope in all the scopes between the current scope
                        // and the original scope.
                        let mut parent_symbol = *original;
                        // Propagate the free symbol from outer to inner.
                        for j in (num_outers - i)..num_outers {
                            let o = &mut self.outers[j];
                            parent_symbol = o.define_free(name, parent_symbol);
                        }
                        Some(self.current.define_free(name, parent_symbol))
                    }
                };
            }
        }
        None
    }

    pub fn num_definitions(&self) -> u16 {
        self.current.num_definitions
    }
}