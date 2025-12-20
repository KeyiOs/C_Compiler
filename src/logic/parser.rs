use core::panic;
use std::str::FromStr;

use crate::{TokenType, data::{AstNode, OperatorPrecedence, Tokens, types::Type}};

pub fn parser_start(input: &mut Tokens, depth: usize) -> Vec<AstNode> {
    let mut ast = Vec::new();

    loop {
        let next_token = input.peek().clone();
        if next_token.token_type == TokenType::EOF {
            break;
        }

        match &next_token.token_type {
            TokenType::Keyword(k) => {
                let tmp = input.next();

                match Type::from_str(k.as_str()) {
                    Ok(_) => {
                        input.tokens.push(tmp);
                        ast.push(process_declaration(input));
                    },
                    Err(_) => {
                        match k.as_str() {
                            "if" => {
                                ast.push(process_if(input, depth, false));
                            }
                            "while" => {
                                ast.push(process_while(input, depth));
                            }
                            "switch" => {
                                ast.push(process_switch(input, depth));
                            }
                            "break" => {
                                if depth == 0 {
                                    panic!("'break' used outside of loop on line {}", input.peek().line);
                                }

                                ast.push(AstNode::Break);
                                input.operator_match(";");
                                break;
                            }
                            "continue" => {
                                if depth == 0 {
                                    panic!("'continue' used outside of loop on line {}", input.peek().line);
                                }
                                
                                ast.push(AstNode::Continue);
                                input.operator_match(";");
                            }
                            "enum" => {
                                ast.push(process_enum(input));
                            }
                            "return" => {
                                if input.peek().token_type.eq(&TokenType::Operator(";".to_string())) {
                                    ast.push(AstNode::Return { expression: None });
                                    input.next();
                                } else {
                                    let expr = process_expression(input, 0, false);
                                    ast.push(AstNode::Return { expression: Some(Box::new(expr)) });
                                    input.operator_match(";");
                                }
                            }
                            _ => {
                                input.tokens.push(tmp);
                                break;
                            }
                        }
                    }
                }
            }
            TokenType::Literal(_) => {
                ast.push(process_expression(input, 0, false));
                input.operator_match(";");
            } TokenType::Identifier(_) => {
                ast.push(process_expression(input, 0, false));
                input.operator_match(";");
            } TokenType::Operator(s) => {
                match s.as_str() {
                    "}" => {
                        break
                    },
                    _ => { panic!("Unexpected operator {} on line {}", s, input.peek().line); }
                }
            }
            _ => {input.next();}
        }
    }

    ast
}


fn process_expression(input: &mut Tokens, l_power: u8, mut comparison: bool) -> AstNode {
    let line = input.peek().line;

    let mut lhs = match input.next().token_type {
        TokenType::Literal(s) => { AstNode::Value(s) },
        TokenType::Identifier(s) => { AstNode::Value(s) },
        TokenType::Operator(ref s) if s == "(" => { 
            let lhs = process_expression(input, 0, comparison);
            if !input.next().token_type.eq(&TokenType::Operator(")".to_string())) {
                panic!("Expected ')' on line {}", line)
            };
            lhs
        },
        TokenType::Keyword(ref s) if s == "true" || s == "false" => {
            AstNode::Value(s.clone())
        },
        _ => panic!("Unexpected token on line {}", line)
    };

    loop {
        let operator = match &input.peek().token_type {
            TokenType::EOF => break,
            TokenType::Operator(s) if s == ")" || s == ";" => break,
            TokenType::Operator(s) => s.clone(),
            _ => panic!("Unexpected token on line {}", input.peek().line)
        };

        let r_power = operator.precedence();
        if r_power == 0 || l_power >= r_power {
            break;
        } else if r_power == 2 {
            if comparison {
                panic!("Chained comparisons are not allowed on line {}", input.peek().line);
            }

            comparison = true;
        }

        input.next();
        let rhs = process_expression(input, r_power, comparison);

        lhs = AstNode::BinaryOperation {
            left: Box::new(lhs),
            operator: operator,
            right: Box::new(rhs),
        }
    }

    lhs
}


fn process_body(input: &mut Tokens, depth: usize) -> Vec<AstNode> {
    input.operator_match("{");
    let body = parser_start(input, depth + 1);
    input.operator_match("}");

    body
}


fn process_data_type(input: &mut Tokens) -> Type {
    let mut var_type;
    let mut sign = Option::<String>::None;

    match input.peek().token_type {
        TokenType::Keyword(ref s) if s == "unsigned" || s == "signed" => {
            sign = Some(input.next().token_type.value().to_string());
        }
        _ => {}
    }
    
    let ttype = input.next();
    if ttype.token_type.eq(&TokenType::Keyword("long".to_string())) {
        if input.peek().token_type.eq(&TokenType::Keyword("long".to_string())) {
            var_type = Type::LongLong;
            input.next();
        } else {
            var_type = Type::Long;
        }
    } else {
        var_type = Type::from_str(ttype.token_type.value()).unwrap();
    }

    if sign.is_some() {
        match var_type {
            Type::Int | Type::Short | Type::Long | Type::LongLong => {
                match sign.unwrap().as_str() {
                    "unsigned" => {
                        var_type = Type::Unsigned(Box::new(var_type));
                    }
                    "signed" => {
                        var_type = Type::Signed(Box::new(var_type));
                    }
                    _ => {}
                }
            }
            _ => {
                panic!("Only integer types can be signed or unsigned on line {}", input.peek().line);
            }
        }
    }

    var_type
}


fn process_declaration(input: &mut Tokens) -> AstNode {
    let var_type = process_data_type(input);

    let identifier = if matches!(input.peek().token_type, TokenType::Identifier(_)) {
        input.next().token_type.value().to_string()
    } else {
        panic!("Declaration missing identifier on line {}", input.peek().line);
    };

    if input.peek().token_type.eq(&TokenType::Operator("(".to_string())) {
        input.next();
        process_fn_declaration(input, var_type, identifier)
    } else {
        match var_type {
            Type::Void => {
                panic!("Void variables are not allowed on line {}", input.peek().line);
            }
            _ => {
                process_var_declaration(input, var_type, identifier)
            }
        }
    }
}


fn process_fn_declaration(input: &mut Tokens, return_type: Type, identifier: String) -> AstNode {
    let mut parameters: Vec<(Type, String)> = Vec::new();
    while input.peek().token_type.ne(&TokenType::Operator(")".to_string())) {
        let param_type = process_data_type(input);

        if param_type == Type::Void {
            if !parameters.is_empty() || !input.peek().token_type.eq(&TokenType::Operator(")".to_string())) {
                panic!("Void type can only be used for single 'void' parameter on line {}", input.peek().line);
            }
            
            parameters.push((param_type, String::new()));
            break;
        }

        let param_identifier = match &input.next().token_type {
            TokenType::Identifier(s) => s.clone(),
            _ => panic!("Expected parameter identifier in function declaration on line {}", input.peek().line)
        };

        if input.peek().token_type.ne(&TokenType::Operator(")".to_string())) {
            input.operator_match(",");
        }

        parameters.push((param_type, param_identifier));
    }

    input.next();

    let tok = input.next();
    match tok.token_type {
        TokenType::Operator(ref s) if s == "{" => {
            input.tokens.push(tok);

            AstNode::FnDefinition {
                return_type,
                identifier,
                parameters,
                body: process_body(input, 0),
            }
        }
        TokenType::Operator(ref s) if s == ";" => {
            AstNode::FnDeclaration {
                return_type,
                identifier,
                parameters,
            }
        }
        _ => panic!("Expected '{{' or ';' after function declaration on line {}, but found {}", tok.line, tok.token_type.value()),
    }
}


fn process_var_declaration(input: &mut Tokens, var_type: Type, identifier: String) -> AstNode {
    let value: Option<Box<AstNode>> = if input.peek().token_type.eq(&TokenType::Operator("=".to_string())) {
        input.next();
        
        if var_type == Type::Bool {
            let next = input.peek();
            match &next.token_type {
                TokenType::Keyword(s) if s == "true" || s == "false" => {
                    Some(Box::new(AstNode::Value(input.next().token_type.value().to_string())))
                }
                _ => {
                    panic!("Expected boolean literal 'true' or 'false' on line {}", next.line);
                }
            }
        } else {
            Some(Box::new(process_expression(input, 0, false)))
        }
    } else {
        None
    };
    
    input.operator_match(";");

    AstNode::VarDeclaration {
        var_type,
        identifier,
        value,
    }
}


fn process_if(input: &mut Tokens, depth: usize, is_else: bool) -> AstNode {
    input.operator_peek("(");

    if is_else {
        return AstNode::ElseStatement {
            condition: Some(Box::new(process_expression(input, 0, false))),
            body: process_body(input, depth),
            else_branch: process_else(input, depth),
        }
    }

    AstNode::IfStatement {
        condition: Box::new(process_expression(input, 0, false)),
        body: process_body(input, depth),
        else_branch: process_else(input, depth),
    }
}


fn process_else(input: &mut Tokens, depth: usize) -> Option<Box<AstNode>> {
    if input.type_match(&TokenType::Keyword("else".to_string())) {
        if input.type_match(&TokenType::Keyword("if".to_string())) {
            Some(Box::new(process_if(input, depth, true)))
        } else {
            let else_body = process_body(input, depth);

            Some(Box::new(AstNode::ElseStatement {
                condition: None,
                body: else_body,
                else_branch: None,
            }))
        }
    } else {
        None
    }
}


fn process_while(input: &mut Tokens, depth: usize) -> AstNode {
    input.operator_peek("(");

    AstNode::WhileStatement {
        condition: Box::new(process_expression(input, 0, false)),
        body: process_body(input, depth),
    }
}


fn process_switch(input: &mut Tokens, depth: usize) -> AstNode {
    input.operator_match("(");

    let next = input.peek();
    let identifier = if let TokenType::Identifier(_) = next.token_type {
        input.next().token_type.value().to_string()
    } else {
        panic!("Switch missing identifier on line {}", next.line);
    };
    
    input.operator_match(")");
    input.operator_match("{");

    let mut case_identifier = String::new();
    let mut cases = Vec::new();
    let mut default = false;
    loop {
        if default {
            panic!("Default case must be the last case in switch on line {}", input.peek().line);
        }

        if input.type_match(&TokenType::Keyword("default".to_string())) {
            case_identifier = "default".to_string();
            default = true;
        } else if !input.type_match(&TokenType::Keyword("case".to_string())) {
            let found = input.peek();
            panic!("Expected 'case' in switch statement on line {} but found {}", found.line, found.token_type.value());
        }

        if !default {
            let next = input.peek();
            if !matches!(next.token_type, TokenType::Identifier(_)) {
                panic!("While loop missing condition on line {}", next.line);
            }

            case_identifier = input.next().token_type.value().to_string();
        }

        input.operator_match(":");

        let body: Vec<AstNode> = parser_start(input, depth + 1);
        if body.is_empty() {
            panic!("Switch must have at least one case on line {}", input.peek().line);
        }

        cases.push(AstNode::Case {
            identifier: case_identifier.clone(),
            body,
        });

        if input.peek().token_type.eq(&TokenType::Operator("}".to_string())) {
            input.next();
            break;
        }
    }

    AstNode::Switch {
        identifier,
        cases,
    }
}


fn process_enum(input: &mut Tokens) -> AstNode {
    let next = input.peek();
    let name = if let TokenType::Identifier(_) = next.token_type {
        input.next().token_type.value().to_string()
    } else {
        panic!("Expected enum name on line {}", next.line);
    };

    input.operator_match("{");

    let mut variants = Vec::new();
    loop {
        if !matches!(input.peek().token_type, TokenType::Identifier(_)) {
            break;
        }

        variants.push(input.next().token_type.value().to_string());
        input.operator_match(",");
    }

    input.operator_match("}");

    if variants.is_empty() {
        panic!("Enum must have at least one variant on line {}", input.peek().line);
    }
    
    AstNode::Enum {
        name,
        variants,
    }
}