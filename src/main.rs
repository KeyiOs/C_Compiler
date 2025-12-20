mod data;
mod logic;

use std::path::Path;
use std::fs;
use std::process::Command;
use std::io::Write;
use data::Token;
use logic::lexer_start;
use logic::parser_start;

use crate::data::AstNode;
use crate::data::TokenType;
use crate::data::Tokens;

const INPUT_CODE: &str = "./examples/test.txt";
const DEBUG: bool = false;
const MAX_TO_PRINT: usize = 20;
const PRINT_TYPE: u8 = 0; // 0=All, 1=Keyword, 2=Operator, 3=Literal, 4=Identifier

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
    if DEBUG {
        preproces_out(preproces_source);
        token_out(tokens.tokens);
    }
    
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
fn preproces_out(preproces_source: String) {
    let output_path = "output/prep_out.c";
    let output_dir = Path::new(output_path).parent().unwrap();

    if let Err(e) = fs::create_dir_all(output_dir) {
        eprintln!("Failed to create directory {:?}: {}", output_dir, e);
        return;
    }

    if let Err(e) = fs::write(output_path, preproces_source) {
        eprintln!("Failed to write preprocessed output: {}", e);
    }
}


fn token_out(tokens: Vec<Token>) {
    let output_path = "output/token_out.txt";
    let output_dir = Path::new(output_path).parent().unwrap();

    if let Err(e) = fs::create_dir_all(output_dir) {
        eprintln!("Failed to create directory {:?}: {}", output_dir, e);
        return;
    }

    let mut file = match fs::File::create(output_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to create output file {}: {}", output_path, e);
            return;
        }
    };

    let mut count = 0;
    for token in tokens.iter().rev() {
        let (type_id, text) = match &token.token_type {
            TokenType::Keyword(s) => (1, s),
            TokenType::Operator(s) => (2, s),
            TokenType::Literal(s) => (3, s),
            TokenType::Identifier(s) => (4, s),
            TokenType::EOF => (5, &String::from("EOF")),
        };

        if PRINT_TYPE != 0 && PRINT_TYPE != type_id {
            continue;
        }

        if let Err(e) = writeln!(file, "{}", text) {
            eprintln!("Failed to write to file: {}", e);
            break;
        }

        count += 1;
        if MAX_TO_PRINT != 0 && count >= MAX_TO_PRINT {
            break;
        }
    }
}


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
        for (i, stmt) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            inner(stmt, &new_prefix, is_last, None);
        }
    }

    fn inner(node: &AstNode, prefix: &str, last: bool, side: Option<&str>) {
        let branch = if last { "└─ " } else { "├─ " };
        print!("{}", prefix);
        print!("{}", branch);

        let side_label = side.map_or("".to_string(), |s| format!(" {}{}", MAGENTA, s));

        match node {
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
                    
                    if is_last {
                        let body_prefix = format!("{}{}", new_prefix, "    ");
                        print_body(body, &body_prefix);
                    }
                }
            }
            AstNode::VarDeclaration { var_type, identifier, value } => {
                println!("{}VarDeclaration ({}{}{}: {}{}{}){}{}", CYAN, RED, identifier, CYAN, RED, format!("{:?}", var_type), CYAN, side_label, RESET);
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
            AstNode::Enum { name, variants } => {
                println!("{}Enum({}{}{}){}", CYAN, RED, name, CYAN, RESET);
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
            AstNode::Return { expression } => {
                println!("{}Return{}", CYAN, RESET);
                if let Some(expr) = expression {
                    let new_prefix = format!("{}{}", prefix, if last { "    " } else { "│   " });
                    inner(expr, &new_prefix, true, None);
                }
            }
            AstNode::Value(v) => {
                println!("{}Value({}{}{}){}{}", GREEN, RED, v, GREEN, side_label, RESET);
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
}
