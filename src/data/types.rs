use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Keyword(String),
    Operator(String),
    Literal(String),
    Identifier(String),
    EOF,
}


#[derive(Debug, Clone, Serialize)]
pub enum AstNode {
    FnDeclaration {
        return_type: Type,
        identifier: String,
        parameters: Vec<(Type, String)>,
    },
    
    FnDefinition {
        return_type: Type,
        identifier: String,
        parameters: Vec<(Type, String)>,
        body: Vec<AstNode>,
    },

    VarDeclaration {
        var_type: Type,
        identifier: String,
        value: Option<Box<AstNode>>,
    },

    BinaryOperation {
        left: Box<AstNode>,
        operator: String,
        right: Box<AstNode>,
    },

    IfStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },

    ElseStatement {
        condition: Option<Box<AstNode>>,
        body: Vec<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },

    WhileStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
    },

    Switch {
        identifier: String,
        cases: Vec<AstNode>,
    },

    Case {
        identifier: String,
        body: Vec<AstNode>,
    },

    Enum {
        name: String,
        variants: Vec<String>,
    },

    Return {
        expression: Option<Box<AstNode>>,
    },

    Break,
    Continue,

    Value(String),
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Bool,       // ✅
    Break,      // ✅
    Case,       // ✅
    Char,       // ✅
    Continue,   // ✅
    Default,    // ✅
    Double,     // ✅
    Else,       // ✅
    Enum,       // ✅
    False,      // ✅
    Float,      // ✅
    For,        // Function
    If,         // ✅
    Int,        // ✅
    Long,       // ✅
    Return,     // ✅
    Short,      // ✅
    Signed,     // ✅
    Struct,     // Function
    Switch,     // ✅
    True,       // ✅
    Unsigned,   // ✅
    Void,       // ✅
    While,      // ✅
}


#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
pub enum Type {
    Void,
    Bool,
    Char,
    Int,
    Float,
    Double,
    Short,
    Long,
    LongLong,
    Unsigned(Box<Type>),
    Signed(Box<Type>),
}


#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SingleOperator {
    Ampersand,
    Asterisk,
    Backslash,
    Caret,
    Colon,
    Comma,
    Dot,
    DoubleQuote,
    Equal,
    Exclamation,
    GreaterThan,
    LessThan,
    Minus,
    ParenthesisLeft,
    ParenthesisRight,
    Percent,
    Pipe,
    Plus,
    Question,
    Semicolon,
    SingleQuote,
    Slash,
    SquareBracketLeft,
    SquareBracketRight,
    Tilde,
    CurlyBracketLeft,
    CurlyBracketRight,
}


#[derive(Debug, PartialEq, Eq, Hash)]
pub enum DoubleOperator {
    DoubleAmpersand,
    DoubleMinus,
    DoublePipe,
    DoublePlus,
    Pointer,
    DoubleGreaterThan,
    DoubleLessThan,
    LessThanEqual,
    GreaterThanEqual,
    DoubleEqual,
    ExclamationEqual,
    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    PercentEqual,
    AmpersandEqual,
    CaretEqual,
    PipeEqual,
}


#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TripleOperator {
    LeftShiftEqual,
    RightShiftEqual,
}