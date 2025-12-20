use crate::Token;
use crate::data::types::Type;
use crate::data::{ TokenType, Tokens };
use core::panic;
use std::str::FromStr;

impl Token {
    pub fn new(token_type: TokenType, line: u16) -> Self {
        Self { token_type, line }
    }
}


impl TokenType {
    pub fn value(&self) -> &str {
        match self {
            TokenType::Keyword(s)
            | TokenType::Operator(s)
            | TokenType::Literal(s)
            | TokenType::Identifier(s) => s.as_str(),
            | TokenType::EOF => "EOF",
        }
    }
}


impl Tokens {
    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap()
    }

    pub fn peek(&self) -> &Token {
        self.tokens.last().unwrap()
    }

    pub fn operator_match(&mut self, other: &str) -> bool {
        let var = self.next();

        if var.token_type.eq(&TokenType::Operator(other.to_string())) {
            true
        } else {
            panic!("Expected '{}' on line {} but found '{}'", other, var.line, var.token_type.value());
        }
    }

    pub fn operator_peek(&self, other: &str) -> bool {
        let var = self.peek();

        if var.token_type.eq(&TokenType::Operator(other.to_string())) {
            true
        } else {
            panic!("Expected '{}' on line {} but found '{}'", other, var.line, var.token_type.value());
        }
    }

    pub fn type_match(&mut self, other: &TokenType) -> bool {
        let var = self.peek();

        if var.token_type.eq(other) {
            self.next();
            true
        } else {
            false
        }
    }
}


impl FromStr for Type {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "void" => Ok(Type::Void),
            "bool" => Ok(Type::Bool),
            "char" => Ok(Type::Char),
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "double" => Ok(Type::Double),
            "long" => Ok(Type::Long),
            "short" => Ok(Type::Short),
            "unsigned" => Ok(Type::Unsigned(Box::new(Type::Int))),
            "signed" => Ok(Type::Signed(Box::new(Type::Int))),
            _ => Err(()),
        }
    }
}


impl OperatorPrecedence for String {
    fn precedence(&self) -> u8 {
        match self.as_str() {
            "++" | "--" => 5,
            "*" | "/" | "%" => 4,
            "+" | "-" => 3,
            ">" | "<" | ">=" | "<=" | "==" | "!=" => 2,
            "+=" | "-=" | "*=" | "/=" | "%=" | "=" => 1,
            _ => 0,
        }
    }
}


pub trait OperatorPrecedence {
    fn precedence(&self) -> u8;
}