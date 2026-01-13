use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Keyword(String),
    Operator(String),
    Literal(String, String),
    Identifier(String),
    EOF,
}


#[derive(Debug, Clone, Serialize)]
pub enum AstNode {
    ArrayAccess {
        array: Box<AstNode>,
        index: Box<AstNode>,
    },

    BinaryOperation {
        left: Box<AstNode>,
        operator: String,
        right: Box<AstNode>,
    },

    Case {
        identifier: String,
        body: Vec<AstNode>,
    },

    ElseStatement {
        condition: Option<Box<AstNode>>,
        body: Vec<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },

    Dereference {
        operand: Box<AstNode>,
    },

    Enum {
        identifier: String,
        variants: Vec<(String, Option<String>)>,
    },

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

    ForStatement {
        declarations: Option<Vec<AstNode>>,
        condition: Option<Box<AstNode>>,
        increments: Option<Vec<AstNode>>,
        body: Vec<AstNode>,
    },

    FunctionCall {
        identifier: String,
        arguments: Vec<AstNode>,
    },

    IfStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
        else_branch: Option<Box<AstNode>>,
    },

    Printf {
        format_string: String,
        arguments: Vec<AstNode>,
    },

    Reference {
        operand: Box<AstNode>,
    },

    Return {
        expression: Option<Box<AstNode>>,
    },

    Struct {
        identifier: String,
        members: Vec<AstNode>,
        variables: Vec<(Type, String, Option<Box<AstNode>>)>,
    },

    StructDeclaration {
        identifier: String,
        struct_name: String,
    },

    Switch {
        identifier: Box<AstNode>,
        cases: Vec<AstNode>,
    },

    UnaryOperation {
        operand: Box<AstNode>,
        operator: String,
    },

    VarDeclaration {
        var_type: Type,
        identifier: String,
        value: Option<Box<AstNode>>,
    },

    ArrayInitializer {
        items: Vec<AstNode>,
    },

    DesignatedInitializer {
        members: Vec<(String, AstNode)>,
    },

    WhileStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
    },

    Break,
    Continue,

    Literal {
        value: String,
        data_type: String,
    },
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Bool,
    Break,
    Case,
    Char,
    Continue,
    Default,
    Double,
    Else,
    Enum,
    False,
    Float,
    For,
    If,
    Int,
    Long,
    Printf,
    Return,
    Short,
    Signed,
    Struct,
    Switch,
    True,
    Unsigned,
    Void,
    While,
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
    Pointer(Box<Type>),
    Array(Box<Type>, Option<String>),
    Struct(String),
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