use std::collections::HashMap;

pub type Sym = u16;
pub type Namespace = HashMap<Sym, i64>;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    map: HashMap<String, Sym>,
    arr: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            map: HashMap::new(),
            arr: Vec::new(),
        }
    }

    pub fn name(&self, sym: Sym) -> &String {
        &self.arr[sym as usize]
    }

    pub fn insert(&mut self, name: &str) -> Sym {
        if let Some(sym) = self.map.get(name) {
            *sym
        } else {
            let sym = self.arr.len() as Sym;
            self.arr.push(String::from(name));
            self.map.insert(String::from(name), sym);
            sym
        }
    }
}
