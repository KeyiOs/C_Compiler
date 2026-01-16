use std::str::FromStr;
use std::fmt;
use std::collections::HashMap;
use std::sync::LazyLock;

use serde::Serialize;


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Keyword(String),
    Operator(String),
    Literal(String, String),
    Identifier(String),
    EOF,
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

pub static KEYWORD_MAP: LazyLock<HashMap<&'static str, Keyword>> = LazyLock::new(|| {
    HashMap::from([
        ("bool", Keyword::Bool),
        ("break", Keyword::Break),
        ("case", Keyword::Case),
        ("char", Keyword::Char),
        ("continue", Keyword::Continue),
        ("default", Keyword::Default),
        ("double", Keyword::Double),
        ("else", Keyword::Else),
        ("enum", Keyword::Enum),
        ("false", Keyword::False),
        ("float", Keyword::Float),
        ("for", Keyword::For),
        ("if", Keyword::If),
        ("int", Keyword::Int),
        ("long", Keyword::Long),
        ("printf", Keyword::Printf),
        ("return", Keyword::Return),
        ("short", Keyword::Short),
        ("signed", Keyword::Signed),
        ("struct", Keyword::Struct),
        ("switch", Keyword::Switch),
        ("true", Keyword::True),
        ("unsigned", Keyword::Unsigned),
        ("void", Keyword::Void),
        ("while", Keyword::While),
    ])
});


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

pub static SINGLE_OPERATOR_MAP: LazyLock<HashMap<char, SingleOperator>> = LazyLock::new(|| {
    HashMap::from([
        ('&', SingleOperator::Ampersand),
        ('*', SingleOperator::Asterisk),
        ('\\', SingleOperator::Backslash),
        ('^', SingleOperator::Caret),
        (':', SingleOperator::Colon),
        (',', SingleOperator::Comma),
        ('.', SingleOperator::Dot),
        ('"', SingleOperator::DoubleQuote),
        ('=', SingleOperator::Equal),
        ('!', SingleOperator::Exclamation),
        ('>', SingleOperator::GreaterThan),
        ('<', SingleOperator::LessThan),
        ('-', SingleOperator::Minus),
        ('(', SingleOperator::ParenthesisLeft),
        (')', SingleOperator::ParenthesisRight),
        ('%', SingleOperator::Percent),
        ('|', SingleOperator::Pipe),
        ('+', SingleOperator::Plus),
        ('?', SingleOperator::Question),
        (';', SingleOperator::Semicolon),
        ('\'', SingleOperator::SingleQuote),
        ('/', SingleOperator::Slash),
        ('[', SingleOperator::SquareBracketLeft),
        (']', SingleOperator::SquareBracketRight),
        ('~', SingleOperator::Tilde),
        ('{', SingleOperator::CurlyBracketLeft),
        ('}', SingleOperator::CurlyBracketRight),
    ])
});


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
    Arrow,
}

pub static DOUBLE_OPERATOR_MAP: LazyLock<HashMap<&str, DoubleOperator>> = LazyLock::new(|| {
    HashMap::from([
        ("&&", DoubleOperator::DoubleAmpersand),
        ("--", DoubleOperator::DoubleMinus),
        ("||", DoubleOperator::DoublePipe),
        ("++", DoubleOperator::DoublePlus),
        (">>", DoubleOperator::DoubleGreaterThan),
        ("<<", DoubleOperator::DoubleLessThan),
        ("<=", DoubleOperator::LessThanEqual),
        (">=", DoubleOperator::GreaterThanEqual),
        ("==", DoubleOperator::DoubleEqual),
        ("!=", DoubleOperator::ExclamationEqual),
        ("+=", DoubleOperator::PlusEqual),
        ("-=", DoubleOperator::MinusEqual),
        ("*=", DoubleOperator::AsteriskEqual),
        ("/=", DoubleOperator::SlashEqual),
        ("%=", DoubleOperator::PercentEqual),
        ("&=", DoubleOperator::AmpersandEqual),
        ("^=", DoubleOperator::CaretEqual),
        ("|=", DoubleOperator::PipeEqual),
        ("->", DoubleOperator::Arrow),
    ])
});


#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TripleOperator {
    LeftShiftEqual,
    RightShiftEqual,
}

pub static TRIPLE_OPERATOR_MAP: LazyLock<HashMap<&str, TripleOperator>> = LazyLock::new(|| {
    HashMap::from([
        ("<<=", TripleOperator::LeftShiftEqual),
        (">>=", TripleOperator::RightShiftEqual),
    ])
});


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

    MemberAccess {
        object: Box<AstNode>,
        member: String,
        is_arrow: bool,
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