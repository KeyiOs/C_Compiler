use std::collections::HashMap;
use std::sync::LazyLock;

use crate::data::{Keyword, types::{ DoubleOperator, SingleOperator, TripleOperator }};


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


pub static TRIPLE_OPERATOR_MAP: LazyLock<HashMap<&str, TripleOperator>> = LazyLock::new(|| {
    HashMap::from([
        ("<<=", TripleOperator::LeftShiftEqual),
        (">>=", TripleOperator::RightShiftEqual),
    ])
});