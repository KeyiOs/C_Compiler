use crate::data::definitions::Type;
use crate::error::{SymbolError, SymbolResult};
use std::collections::HashMap;


#[derive(Debug, Clone)]
pub enum Symbol {
    Function {
        name: String,
        return_type: Type,
        parameters: Vec<(Type, String)>,
        is_initialized: bool,
    },
    Variable {
        name: String,
        var_type: Type,
        is_initialized: bool,
    },
}


#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
}


impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn insert_function(
        &mut self,
        name: String,
        return_type: Type,
        parameters: Vec<(Type, String)>,
        is_initialized: bool,
    ) -> SymbolResult<()> {
        if self.symbols.contains_key(&name) {
            return Err(SymbolError::SymbolAlreadyExists { name });
        }

        self.symbols.insert(
            name.clone(),
            Symbol::Function {
                name,
                return_type,
                parameters,
                is_initialized,
            },
        );
        Ok(())
    }

    pub fn insert_variable(
        &mut self,
        name: String,
        var_type: Type,
        is_initialized: bool,
    ) -> SymbolResult<()> {
        if self.symbols.contains_key(&name) {
            return Err(SymbolError::SymbolAlreadyExists { name });
        }

        self.symbols.insert(
            name.clone(),
            Symbol::Variable {
                name,
                var_type,
                is_initialized,
            },
        );
        Ok(())
    }

    pub fn remove(&mut self, name: &str) -> SymbolResult<()> {
        if self.symbols.remove(name).is_none() {
            return Err(SymbolError::SymbolNotFound { name: name.to_string() });
        }
        Ok(())
    }

    pub fn get_name(&self, name: &str) -> SymbolResult<String> {
        match self.symbols.get(name) {
            Some(symbol) => match symbol {
                Symbol::Function { name, .. } => Ok(name.clone()),
                Symbol::Variable { name, .. } => Ok(name.clone()),
            },
            None => Err(SymbolError::SymbolNotFound { name: name.to_string() }),
        }
    }

    pub fn get_type(&self, name: &str) -> SymbolResult<Type> {
        match self.symbols.get(name) {
            Some(symbol) => match symbol {
                Symbol::Function { return_type, .. } => Ok(return_type.clone()),
                Symbol::Variable { var_type, .. } => Ok(var_type.clone()),
            },
            None => Err(SymbolError::SymbolNotFound { name: name.to_string() }),
        }
    }

    pub fn get_parameters(&self, name: &str) -> SymbolResult<Vec<(Type, String)>> {
        match self.symbols.get(name) {
            Some(Symbol::Function { parameters, .. }) => Ok(parameters.clone()),
            Some(Symbol::Variable { .. }) => {
                Err(SymbolError::NotAFunction { name: name.to_string() })
            }
            None => Err(SymbolError::SymbolNotFound { name: name.to_string() }),
        }
    }

    pub fn is_initialized(&self, name: &str) -> SymbolResult<bool> {
        match self.symbols.get(name) {
            Some(Symbol::Function { is_initialized, .. }) => Ok(*is_initialized),
            Some(Symbol::Variable { is_initialized, .. }) => Ok(*is_initialized),
            None => Err(SymbolError::SymbolNotFound { name: name.to_string() }),
        }
    }

    pub fn set_initialized(&mut self, name: &str, initialized: bool) -> SymbolResult<()> {
        match self.symbols.get_mut(name) {
            Some(Symbol::Function {
                is_initialized, ..
            }) => {
                *is_initialized = initialized;
                Ok(())
            }
            Some(Symbol::Variable {
                is_initialized, ..
            }) => {
                *is_initialized = initialized;
                Ok(())
            }
            None => Err(SymbolError::SymbolNotFound { name: name.to_string() }),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn get_all(&self) -> Vec<&Symbol> {
        self.symbols.values().collect()
    }

    pub fn clear(&mut self) {
        self.symbols.clear();
    }
}