use crate::Token;
use crate::data::types::Type;
use crate::data::{ TokenType, Tokens };
use crate::error::{ParseError, ParseResult};
use std::str::FromStr;
use std::fmt;

impl Token {
    #[inline]
    pub fn new(token_type: TokenType, line: u16) -> Self {
        Self { token_type, line }
    }
}


impl TokenType {
    #[inline]
    pub fn value(&self) -> &str {
        match self {
            TokenType::Keyword(s)
            | TokenType::Operator(s)
            | TokenType::Literal(s, _)
            | TokenType::Identifier(s) => s.as_str(),
            | TokenType::EOF => "EOF",
        }
    }
}


impl Tokens {
    #[inline]
    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap()
    }

    #[inline]
    pub fn peek(&self) -> &Token {
        self.tokens.last().unwrap()
    }

    pub fn operator_match(&mut self, other: &str) -> ParseResult<()> {
        let var = self.next();

        if var.token_type.eq(&TokenType::Operator(other.to_string())) {
            Ok(())
        } else {
            Err(ParseError::ExpectedOperator { 
                expected: other.to_string(), 
                found: var.token_type.value().to_string(), 
                line: var.line 
            })
        }
    }

    pub fn operator_peek(&self, other: &str) -> ParseResult<()> {
        let var = self.peek();

        if var.token_type.eq(&TokenType::Operator(other.to_string())) {
            Ok(())
        } else {
            Err(ParseError::ExpectedOperator { 
                expected: other.to_string(), 
                found: var.token_type.value().to_string(), 
                line: var.line 
            })
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


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Void => write!(f, "Void"),
            Type::Bool => write!(f, "Bool"),
            Type::Char => write!(f, "Char"),
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Double => write!(f, "Double"),
            Type::Short => write!(f, "Short"),
            Type::Long => write!(f, "Long"),
            Type::LongLong => write!(f, "LongLong"),
            Type::Unsigned(inner) => write!(f, "Unsigned({})", inner),
            Type::Signed(inner) => write!(f, "Signed({})", inner),
            Type::Pointer(inner) => write!(f, "Pointer({})", inner),
            Type::Array(inner, size) => {
                match size {
                    Some(s) => write!(f, "Array({}, Size[{}])", inner, s),
                    None => write!(f, "Array({}, None)", inner),
                }
            }
            Type::Struct(name) => write!(f, "Struct(\"{}\")", name),
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
            "+=" | "-=" | "*=" | "/=" | "%=" | "=" | "&&" | "||"=> 1,
            _ => 0,
        }
    }
}


pub trait OperatorPrecedence {
    fn precedence(&self) -> u8;
}