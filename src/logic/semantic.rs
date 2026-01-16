use crate::data::{AstNode, SymbolTable};
use crate::error::{SemanticError, SemanticResult};


pub fn semantic_analyze(ast: &[AstNode]) -> SemanticResult<()> {
    let table = &mut SymbolTable::new();
    
    for node in ast {
        analyze_node(node, table)?;
    }

    Ok(())
}


fn analyze_node(node: &AstNode, table: &mut SymbolTable) -> SemanticResult<()> {
    match node {
        AstNode::FnDeclaration { return_type, identifier, parameters, } => {
            check_identifier(identifier, table, false)?;
            // Insert function into symbol table
        }
        _ => {}
    }

    Ok(())
}


fn check_identifier(identifier: &str, table: &SymbolTable, must_exist: bool) -> SemanticResult<()> {
    let exists = table.contains(identifier);
    
    if must_exist && !exists {
        return Err(SemanticError::UndeclaredIdentifier(identifier.to_string()));
    }
    
    if !must_exist && exists {
        return Err(SemanticError::DuplicateIdentifier(identifier.to_string()));
    }
    
    Ok(())
}