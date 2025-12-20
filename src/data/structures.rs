use crate::data::TokenType;


#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u16,
}


pub struct Tokens {
    pub tokens: Vec<Token>,
}