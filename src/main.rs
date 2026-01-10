mod data;
mod logic;

use std::process::Command;
use data::Token;
use logic::lexer_start;
use logic::parser_start;

use crate::data::AstNode;
use crate::data::TokenType;
use crate::data::Tokens;

const INPUT_CODE: &str = "./examples/test.txt";

fn main() {
    let preproces_source = match preproces_source(INPUT_CODE) {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Preprocessing failed: {}", e);
            return;
        }
    };

    let mut tokens = match lexer_start(&preproces_source) {
        Ok(tokens) => Tokens { tokens },
        Err(e) => {
            println!("\nError: {:?}\n", e);
            return;
        }
    };
    
    let ast = parser_start(&mut tokens, 0);

    /* - DEGUB - */
    let json = serde_json::to_string_pretty(&ast).unwrap();
    println!("{}\n\n\n", json);

    print_ast(&ast);
}


fn preproces_source(file_path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let output = Command::new("cpp")
        .arg(file_path)
        .output()?;

    if !output.status.success() {
        return Err(format!("Preprocessor failed with code {:?}", output.status.code()).into());
    }

    Ok(String::from_utf8(output.stdout)?)
}


/* * * * * * * */
/*  - DEGUB -  */
/* * * * * * * */
const CYAN: &str = "\x1b[1;36m";
const YELLOW: &str = "\x1b[1;33m";
const GREEN: &str = "\x1b[1;32m";
const MAGENTA: &str = "\x1b[1;35m";
const RED: &str = "\x1b[1;31m";
const RESET: &str = "\x1b[0m";

pub fn print_ast(nodes: &[AstNode]) {
    fn print_name(name: &str, prefix: &str, last: bool) -> String {
        println!("{}{}{}", CYAN, name, RESET);
        format!("{}{}", prefix, if last { "    " } else { "│   " })
    }

    fn print_body(body: &[AstNode], new_prefix: &str) {
        let len = body.len();
        for (i, stmt) in body.iter().enumerate() {
            let is_last = i == len - 1;
            inner(stmt, &new_prefix, is_last, None);
        }
    }

    fn inner(node: &AstNode, prefix: &str, last: bool, side: Option<&str>) {
        let branch = if last { "└─ " } else { "├─ " };
        print!("{}", prefix);
        print!("{}", branch);

        let side_label = side.map_or("".to_string(), |s| format!(" {}{}", MAGENTA, s));

        match node {
            AstNode::ArrayAccess { array, index } => {
                println!("{}ArrayAccess{}{}", YELLOW, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                inner(array, &new_prefix, false, Some("array"));
                inner(index, &new_prefix, true, Some("index"));
            }
            AstNode::IfStatement { condition, body, else_branch } => {
                let new_prefix = print_name("IfStatement", prefix, last);

                inner(condition, &new_prefix, body.is_empty() && else_branch.is_none(), None);

                for (i, stmt) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1 && else_branch.is_none();
                    inner(stmt, &new_prefix, is_last, None);
                }

                if let Some(else_node) = else_branch {
                    inner(else_node, &new_prefix, true, None);
                }
            }
            AstNode::ForStatement { declarations, condition, increments, body } => {
                println!("{}ForStatement{}", CYAN, RESET);

                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                if let Some(decls) = declarations {
                    for decl in decls {
                        inner(decl, &new_prefix, false, None);
                    }
                }

                if let Some(cond) = condition {
                    inner(cond, &new_prefix, false, None);
                }

                if let Some(incrs) = increments {
                    for incr in incrs {
                        inner(incr, &new_prefix, body.is_empty(), None);
                    }
                }

                print_body(body, &new_prefix);
            }
            AstNode::FnDeclaration { return_type, identifier, parameters } => {
                println!("{}FnDeclaration ({}{}{}: {}{:?}{}){}{}", CYAN, RED, identifier, CYAN, RED, return_type, CYAN, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                for (i, (param_type, param_name)) in parameters.iter().enumerate() {
                    let is_last = i == parameters.len() - 1;
                    println!("{}{}{}Parameter ({}{}{}: {}{:?}{}){}", new_prefix, if is_last { "└─ " } else { "├─ " }, GREEN, RED, param_name, GREEN, RED, param_type, GREEN, RESET);
                }
            }
            AstNode::FnDefinition { return_type, identifier, parameters, body } => {
                println!("{}FnDefinition ({}{}{}: {}{:?}{}){}{}", CYAN, RED, identifier, CYAN, RED, return_type, CYAN, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                for (i, (param_type, param_name)) in parameters.iter().enumerate() {
                    let is_last = i == parameters.len() - 1;
                    println!("{}{}{}Parameter ({}{}{}: {}{:?}{}){}", new_prefix, if is_last { "└─ " } else { "├─ " }, GREEN, RED, param_name, GREEN, RED, param_type, GREEN, RESET);
                }

                if !parameters.is_empty() {
                    let body_prefix = format!("{}    ", new_prefix);
                    print_body(body, &body_prefix);
                } else {
                    print_body(body, &new_prefix);
                }
            }
            AstNode::VarDeclaration { var_type, identifier, value } => {
                println!("{}VarDeclaration ({}{:?}{}: {}{}{}){}{}", CYAN, RED, identifier, CYAN, RED, format!("{:?}", var_type), CYAN, side_label, RESET);
                if let Some(val) = value {
                    let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                    inner(val, &new_prefix, true, None);
                }
            }
            AstNode::ElseStatement { condition, body, else_branch } => {
                let new_prefix = print_name("ElseStatement", prefix, last);

                if let Some(cond) = condition {
                    inner(cond, &new_prefix, body.is_empty() && else_branch.is_none(), None);
                }

                for (i, stmt) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1 && else_branch.is_none();
                    inner(stmt, &new_prefix, is_last, None);
                }

                if let Some(else_node) = else_branch {
                    inner(else_node, &new_prefix, true, None);
                }
            }
            AstNode::WhileStatement { condition, body} => {
                let new_prefix = print_name("WhileStatement", prefix, last);
                inner(condition, &new_prefix, body.is_empty(), None);
                print_body(body, &new_prefix);
            }
            AstNode::Switch { identifier, cases } => {
                println!("{}Switch({}{}{}){}", CYAN, RED, identifier, CYAN, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                for (i, case) in cases.iter().enumerate() {
                    let is_last = i == cases.len() - 1;
                    inner(case, &new_prefix, is_last, None);
                }
            }
            AstNode::Case { identifier, body } => {
                println!("{}Case({}{}{}){}", CYAN, RED, identifier, CYAN, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                print_body(body, &new_prefix);
            }
            AstNode::Struct { identifier, members, variables } => {
                println!("{}Struct({}{}{}){}", CYAN, RED, identifier, CYAN, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                
                for (i, member) in members.iter().enumerate() {
                    let is_last = i == members.len() - 1 && variables.is_empty();
                    inner(member, &new_prefix, is_last, None);
                }

                for (i, var) in variables.iter().enumerate() {
                    let is_last = i == variables.len() - 1;
                    println!("{}{}{}Variable({}{}{}){}", new_prefix, if is_last { "└─ " } else { "├─ " }, GREEN, RED, var, GREEN, RESET);
                }
            }
            AstNode::StructDeclaration { struct_name, identifier } => {
                println!("{}StructDeclaration ({}{}{}: {}{}{}){}{}", CYAN, RED, identifier, CYAN, RED, struct_name, CYAN, side_label, RESET);
            }
            AstNode::Enum { identifier, variants } => {
                println!("{}Enum({}{}{}){}", CYAN, RED, identifier, CYAN, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                for (i, variant) in variants.iter().enumerate() {
                    let is_last = i == variants.len() - 1;
                    println!("{}{}{}Variant({}{}{}){}", new_prefix, if is_last { "└─ " } else { "├─ " }, GREEN, RED, variant, GREEN, RESET);
                }
            }
            AstNode::BinaryOperation { left, operator, right } => {
                println!("{}BinaryOperation ({}{}{}){}{}", YELLOW, RED, operator, YELLOW, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                inner(left, &new_prefix, false, Some("L"));
                inner(right, &new_prefix, true, Some("R"));
            }
            AstNode::UnaryOperation { operand, operator } => {
                println!("{}UnaryOperation ({}{}{}){}{}", YELLOW, RED, operator, YELLOW, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                inner(operand, &new_prefix, true, None);
            }
            AstNode::FunctionCall { identifier, arguments } => {
                println!("{}FunctionCall ({}{}{}){}{}", YELLOW, RED, identifier, YELLOW, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                for (i, arg) in arguments.iter().enumerate() {
                    let is_last = i == arguments.len() - 1;
                    inner(arg, &new_prefix, is_last, None);
                }
            }
            AstNode::Printf { format_string, arguments } => {
                println!("{}Printf ({}\"{}\"{}){}{}", YELLOW, RED, format_string, YELLOW, side_label, RESET);
                let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                for (i, arg) in arguments.iter().enumerate() {
                    let is_last = i == arguments.len() - 1;
                    inner(arg, &new_prefix, is_last, None);
                }
            }
            AstNode::Return { expression } => {
                println!("{}Return{}", CYAN, RESET);
                if let Some(expr) = expression {
                    let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                    inner(expr, &new_prefix, true, None);
                }
            }
            AstNode::Literal { value, data_type } => {
                println!("{}Literal({}{}{}: {}{}{}){}{}", GREEN, RED, value, GREEN, RED, data_type, GREEN, side_label, RESET);
            }
            AstNode::Break => {
                println!("{}Break{}", GREEN, RESET);
            }
            AstNode::Continue => {
                println!("{}Continue{}", GREEN, RESET);
            }
        }
    }

    for (i, node) in nodes.iter().enumerate() {
        inner(node, "", i == nodes.len() - 1, None);
    }

    println!("{}", RESET);
}
