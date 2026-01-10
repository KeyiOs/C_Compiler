use core::panic;
use std::str::FromStr;

use crate::{TokenType, data::{AstNode, OperatorPrecedence, Token, Tokens, types::Type}};

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
                        ast.extend(process_declaration(input, depth));
                    },
                    Err(_) => {
                        match k.as_str() {
                            "if" => ast.push(process_if(input, depth, false)),
                            "struct" => {
                                if input.tokens.len() >= 2 {
                                    let peek1 = &input.tokens[input.tokens.len() - 2].token_type;
                                    if let TokenType::Identifier(_) = peek1 {
                                        let peek2 = &input.tokens[input.tokens.len() - 3].token_type;
                                        if peek2 == &TokenType::Operator("{".to_string()) {
                                            ast.push(process_struct(input));
                                        } else {
                                            input.tokens.push(tmp);
                                            ast.extend(process_declaration(input, depth));
                                        }
                                    } else {
                                        ast.push(process_struct(input));
                                    }
                                } else {
                                    ast.push(process_struct(input));
                                }
                            }
                            "for" => ast.push(process_for(input, depth)),
                            "while" => ast.push(process_while(input, depth)),
                            "switch" => ast.push(process_switch(input, depth)),
                            "enum" => ast.push(process_enum(input)),
                            "printf" => ast.push(process_printf(input)),
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
            TokenType::Literal(_, _) => {
                ast.push(process_expression(input, 0, false));
                input.operator_match(";");
            } TokenType::Identifier(_) => {
                ast.push(process_expression(input, 0, false));
                input.operator_match(";");
            } TokenType::Operator(s) => {
                match s.as_str() {
                    "}" => break,
                    _ => panic!("Unexpected operator {} on line {}", s, input.peek().line)
                }
            }
            _ => { input.next(); }
        }
    }

    ast
}


fn get_identifier(token: Token) -> String {
    match token.token_type {
        TokenType::Identifier(ref s) => s.to_string(),
        _ => panic!("Expected identifier on line {}", token.line)
    }
}


fn parse_pointer_prefix(input: &mut Tokens, base_type: Type) -> Type {
    let mut current_type = base_type;
    while input.peek().token_type.eq(&TokenType::Operator("*".to_string())) {
        input.next();
        current_type = Type::Pointer(Box::new(current_type));
    }
    current_type
}


fn parse_array_suffix(input: &mut Tokens, base_type: Type) -> Type {
    let mut current_type = base_type;
    while input.peek().token_type.eq(&TokenType::Operator("[".to_string())) {
        input.next();
        let size = if input.peek().token_type.eq(&TokenType::Operator("]".to_string())) {
            None
        } else {
            Some(input.next().token_type.value().to_string())
        };
        input.operator_match("]");
        current_type = Type::Array(Box::new(current_type), size);
    }
    current_type
}


fn expect_comma_or_semicolon(input: &mut Tokens) -> bool {
    match input.next().token_type {
        TokenType::Operator(ref s) if s == "," => true,
        TokenType::Operator(ref s) if s == ";" => false,
        _ => panic!("Expected ',' or ';' on line {}", input.peek().line)
    }
}


fn parse_comma_separated_expressions(input: &mut Tokens, terminator: &str) -> Vec<AstNode> {
    let mut items = Vec::new();
    while input.peek().token_type.ne(&TokenType::Operator(terminator.to_string())) {
        if input.peek().token_type.eq(&TokenType::Operator(",".to_string())) {
            input.next();
        }
        items.push(process_expression(input, 0, false));
    }
    items
}


fn process_body(input: &mut Tokens, depth: usize) -> Vec<AstNode> {
    input.operator_match("{");
    let body = parser_start(input, depth + 1);
    input.operator_match("}");

    body
}


fn process_expression(input: &mut Tokens, l_power: u8, mut comparison: bool) -> AstNode {
    let line = input.peek().line;

    let mut lhs = match input.next().token_type {
        TokenType::Literal(s, t) => AstNode::Literal { value: s, data_type: t },
        TokenType::Identifier(s) => {
            if input.peek().token_type == TokenType::Operator("(".to_string()) {
                process_function_call(input, s)
            } else if input.peek().token_type == TokenType::Operator("[".to_string()) {
                process_array_access(input, s)
            } else {
                AstNode::Literal { value: s, data_type: "identifier".to_string() }
            }
        }
        TokenType::Operator(ref s) if s == "(" => { 
            let lhs = process_expression(input, 0, comparison);
            if !input.next().token_type.eq(&TokenType::Operator(")".to_string())) {
                panic!("Expected ')' on line {}", line)
            };
            lhs
        },
        TokenType::Operator(ref s) if s == "*" => {
            let operand = process_expression(input, 5, comparison);
            AstNode::UnaryOperation { operand: Box::new(operand), operator: "*".to_string() }
        },
        TokenType::Operator(ref s) if s == "&" => {
            let operand = process_expression(input, 5, comparison);
            AstNode::UnaryOperation { operand: Box::new(operand), operator: "&".to_string() }
        },
        TokenType::Keyword(ref s) if s == "true" || s == "false" => AstNode::Literal { value: s.clone(), data_type: "bool".to_string() },
        _ => panic!("Unexpected token on line {}, found {}", input.peek().line, input.peek().token_type.value())
    };

    loop {
        let operator = match &input.peek().token_type {
            TokenType::EOF => break,
            TokenType::Operator(s) if s == ")" || s == ";" => break,
            TokenType::Operator(s) => s.clone(),
            _ => panic!("Unexpected token on line {}, found {}", input.peek().line, input.peek().token_type.value())
        };

        if operator == "++" || operator == "--" {
            input.next();
            return AstNode::UnaryOperation{ operand: Box::new(lhs), operator };
        } else if operator == "&&" || operator == "||" {
            comparison = false;
        }

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


fn process_array_access(input: &mut Tokens, identifier: String) -> AstNode {
    let mut array = AstNode::Literal { value: identifier, data_type: "identifier".to_string() };
    
    while input.peek().token_type.eq(&TokenType::Operator("[".to_string())) {
        input.next();
        let index = process_expression(input, 0, false);
        input.operator_match("]");
        
        array = AstNode::ArrayAccess {
            array: Box::new(array),
            index: Box::new(index),
        };
    }
    
    array
}


fn process_function_call(input: &mut Tokens, identifier: String) -> AstNode {
    input.operator_match("(");
    let arguments = parse_comma_separated_expressions(input, ")");
    input.next();

    AstNode::FunctionCall {
        identifier,
        arguments,
    }
}


fn process_printf(input: &mut Tokens) -> AstNode {
    input.operator_match("(");

    let format_string = match input.next().token_type {
        TokenType::Literal(s, t) if t == "string" => s,
        _ => panic!("Expected format string literal in printf on line {}", input.peek().line)
    };

    let arguments = parse_comma_separated_expressions(input, ")");
    input.next();
    input.operator_match(";");

    AstNode::Printf {
        format_string,
        arguments,
    }
}


fn process_data_type(input: &mut Tokens) -> Type {
    let mut var_type;
    let mut sign = Option::<String>::None;

    match input.peek().token_type {
        TokenType::Keyword(ref s) if s == "unsigned" || s == "signed" => sign = Some(input.next().token_type.value().to_string()),
        _ => {}
    }
    
    let ttype = input.next();
    if ttype.token_type.eq(&TokenType::Keyword("struct".to_string())) {
        let struct_name = get_identifier(input.next());
        var_type = Type::Struct(struct_name);
    } else if ttype.token_type.eq(&TokenType::Keyword("long".to_string())) {
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
            Type::Int | Type::Short | Type::Long | Type::LongLong | Type::Char => {
                match sign.unwrap().as_str() {
                    "unsigned" =>  var_type = Type::Unsigned(Box::new(var_type)),
                    "signed" => var_type = Type::Signed(Box::new(var_type)),
                    _ => {}
                }
            }
            _ => panic!("Only integer types can be signed or unsigned on line {}", input.peek().line)
        }
    }

    var_type
}


fn process_declaration(input: &mut Tokens, depth: usize) -> Vec<AstNode> {
    let mut var_type = process_data_type(input);
    
    let is_pointer_return = input.peek().token_type.eq(&TokenType::Operator("*".to_string()));
    if is_pointer_return && input.tokens.len() >= 2 {
        let peek_ahead = &input.tokens[input.tokens.len() - 2].token_type;
        if let TokenType::Identifier(_) = peek_ahead {
            input.next();
            var_type = Type::Pointer(Box::new(var_type));
        }
    }

    if let [.., second_last, _last] = input.tokens.as_slice() {
        if second_last.token_type == TokenType::Operator("(".to_string()) {
            vec![process_fn_declaration(input, var_type, depth)]
        } else {
            match var_type {
                Type::Void => panic!("Void variables are not allowed on line {}", input.peek().line),
                _ => process_var_declaration(input, var_type)
            }
        }
    } else {
        panic!("Unexpected end of tokens while processing declaration on line {}", input.peek().line);
    }
}


fn process_var_declaration(input: &mut Tokens, var_type: Type) -> Vec<AstNode> {
    let mut declarations = Vec::new();

    loop {
        let mut current_type = parse_pointer_prefix(input, var_type.clone());
        let identifier = get_identifier(input.next());
        current_type = parse_array_suffix(input, current_type);

        let value = if input.peek().token_type.eq(&TokenType::Operator("=".to_string())) {
            input.next();
            Some(Box::new(process_expression(input, 0, false)))
        } else {
            None
        };

        declarations.push(AstNode::VarDeclaration {
            var_type: current_type,
            identifier,
            value,
        });

        if !expect_comma_or_semicolon(input) {
            break;
        }
    }

    declarations
}


fn process_fn_declaration(input: &mut Tokens, return_type: Type, depth: usize) -> AstNode {
    let return_type = parse_pointer_prefix(input, return_type);
    let identifier = get_identifier(input.next());

    input.operator_match("(");

    let mut parameters: Vec<(Type, String)> = Vec::new();
    while input.peek().token_type.ne(&TokenType::Operator(")".to_string())) {
        let base_type = process_data_type(input);
        let mut param_type = parse_pointer_prefix(input, base_type);

        if param_type == Type::Void {
            if !parameters.is_empty() || !input.peek().token_type.eq(&TokenType::Operator(")".to_string())) {
                panic!("Void type can only be used for single 'void' parameter on line {}", input.peek().line);
            }
            
            parameters.push((param_type, String::new()));
            break;
        }

        let param_identifier = get_identifier(input.next());
        param_type = parse_array_suffix(input, param_type);

        if input.peek().token_type.ne(&TokenType::Operator(")".to_string())) {
            input.operator_match(",");
        }

        parameters.push((param_type, param_identifier));
    }

    input.next();

    let tok = input.peek();
    match tok.token_type {
        TokenType::Operator(ref s) if s == "{" => {
            AstNode::FnDefinition {
                return_type,
                identifier,
                parameters,
                body: process_body(input, depth + 1),
            }
        }
        TokenType::Operator(ref s) if s == ";" => {
            input.next();

            AstNode::FnDeclaration {
                return_type,
                identifier,
                parameters,
            }
        }
        _ => panic!("Expected '{{' or ';' after function declaration on line {}, but found {}", tok.line, tok.token_type.value())
    }
}


fn process_if(input: &mut Tokens, depth: usize, is_else: bool) -> AstNode {
    input.operator_match("(");

    if is_else {
        let condition = Some(Box::new(process_expression(input, 0, false)));

        input.operator_match(")");

        return AstNode::ElseStatement {
            condition,
            body: process_body(input, depth),
            else_branch: process_else(input, depth),
        }
    }

    let condition = Box::new(process_expression(input, 0, false));

    input.operator_match(")");

    AstNode::IfStatement {
        condition,
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


fn process_for(input: &mut Tokens, depth: usize) -> AstNode {
    let mut declarations: Option<Vec<AstNode>>;
    let condition: Option<Box<AstNode>>;
    let mut increments: Option<Vec<AstNode>>;

    input.operator_match("(");

    if input.peek().token_type.eq(&TokenType::Operator(";".to_string())) {
        declarations = None;
        input.next();
    } else {
        if !matches!(input.peek().token_type, TokenType::Identifier(_)) {
            let var_type = process_data_type(input);
            declarations = Some(process_var_declaration(input, var_type));
        } else {
            declarations = Some(Vec::new());
            loop {
                if let Some(ref mut vec) = declarations {
                    vec.push(process_expression(input, 0, false));
                }

                match input.peek().token_type {
                    TokenType::Operator(ref s) if s == "," => { input.next(); }
                    TokenType::Operator(ref s) if s == ";" => {
                        input.next();
                        break;
                    }
                    _ => panic!("Expected ',' or ';' after for loop declaration but found {} on line {}", input.peek().token_type.value(), input.peek().line)
                }
            }
        }
    }

    if input.peek().token_type.eq(&TokenType::Operator(";".to_string())) {
        condition = None;
        input.next();
    } else {
        condition = Some(Box::new(process_expression(input, 0, false)));
        input.operator_match(";");
    }

    if input.peek().token_type.ne(&TokenType::Operator(")".to_string())) {
        increments = Some(Vec::new());

        while input.peek().token_type.ne(&TokenType::Operator(")".to_string())) {
            if let Some(ref mut vec) = increments {
                vec.push(process_expression(input, 0, false));
            }

            let token = input.peek();
            match token.token_type {
                TokenType::Operator(ref s) if s == "," => { input.next(); }
                TokenType::Operator(ref s) if s == ")" => {}
                _ => panic!("Expected ',' or ')' after increment but found {} on line {}", token.token_type.value(), token.line)
            }
        }
    } else {
        increments = None;
    }

    input.next();

    AstNode::ForStatement {
        declarations,
        condition,
        increments,
        body: process_body(input, depth + 1),
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

    let identifier = get_identifier(input.next());
    
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
            if !matches!(next.token_type, TokenType::Identifier(_) | TokenType::Literal(_, _)) {
                panic!("Expected case identifier on line {}, but found {}", next.line, next.token_type.value());
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


fn process_struct(input: &mut Tokens) -> AstNode {
    let identifier = get_identifier(input.next());
    let members;
    let mut variables = Vec::new();

    let next = input.next();
    match next.token_type {
        TokenType::Operator(ref s) if s == "{"=> {
            members = process_struct_members(input);

            if input.peek().token_type.ne(&TokenType::Operator(";".to_string())) {
                loop {
                    match input.next().token_type {
                        TokenType::Identifier(s) => variables.push(s),
                        _ => panic!("Expected identifier or ';' in struct variable list on line {}", input.peek().line)
                    }

                    match input.next().token_type {
                        TokenType::Operator(ref s) if s == "," => {}
                        TokenType::Operator(ref s) if s == ";" => break,
                        _ => panic!("Expected ',' or ';' in struct variable list on line {}", input.peek().line)
                    }
                }
            } else {
                input.next();
            }
        }
        TokenType::Identifier(ref s) => {
            input.operator_match(";");
            return AstNode::StructDeclaration { struct_name: identifier, identifier: s.clone() };
        }
        _ => panic!("Unexpected token on line {}", next.line)
    }

    AstNode::Struct {
        identifier,
        members,
        variables
    }
}


fn process_struct_members(input: &mut Tokens) -> Vec<AstNode> {
    let mut members = Vec::new();

    loop {
        if input.peek().token_type.eq(&TokenType::Operator("}".to_string())) {
            input.next();
            break;
        } else if input.peek().token_type.eq(&TokenType::EOF) {
            panic!("Unexpected end of file while parsing struct members on line {}", input.peek().line);
        }

        let mut var_type = process_data_type(input);
        
        if input.peek().token_type.eq(&TokenType::Operator("*".to_string())) {
            input.next();
            var_type = Type::Pointer(Box::new(var_type));
        }
        
        members.push(AstNode::VarDeclaration {
            var_type,
            identifier: get_identifier(input.next()),
            value: None,
        });

        input.operator_match(";");
    }

    members
}


fn process_enum(input: &mut Tokens) -> AstNode {
    let identifier = get_identifier(input.next());

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
    input.operator_match(";");

    if variants.is_empty() {
        panic!("Enum must have at least one variant on line {}", input.peek().line);
    }
    
    AstNode::Enum {
        identifier,
        variants,
    }
}