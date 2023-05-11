use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Let,
    Const,
    True,
    False,
    If,
    For,
    Else,
    Fn,
    Return,
    Match,
    When,
    Then,
    Range,
    Either,
    As,
    In,
    End,
    Next,
    Default,
    Comment(String),
    FnIdent(String),
    VecAcc(String),

    And,
    Or,

    // Symbols
    Equal,
    NotEqual,
    Lt,
    LEt,
    Gt,
    GEt,
    Bang,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Modulo,

    DoubleSlash,

    Assign,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    // Identifiers and literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Vector(String),

    IntType,
    FloatType,
    StringType,
    BoolType,

    // End of file
    EOF,

    // Temp token for match exhaustion
    Err,
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Let => write!(f, "let"),
            Token::Const => write!(f, "const"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::For => write!(f, "for"),
            Token::Else => write!(f, "else"),
            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::Match => write!(f, "match"),
            Token::When => write!(f, "when"),
            Token::Then => write!(f, "then"),
            Token::Range => write!(f, "range"),
            Token::Either => write!(f, "either"),
            Token::As => write!(f, "as"),
            Token::In => write!(f, "in"),
            Token::End => write!(f, "end"),
            Token::Next => write!(f, "next"),
            Token::Default => write!(f, "default"),
            Token::Comment(ref content) => write!(f, "//{}", content),

            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),

            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::LEt => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::GEt => write!(f, ">="),
            Token::Bang => write!(f, "!"),

            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Modulo => write!(f, "%"),

            Token::DoubleSlash => write!(f, "//"),

            Token::Assign => write!(f, "="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),

            Token::Identifier(ref content) => write!(f, "{}", content),
            Token::Integer(value) => write!(f, "{}", value),
            Token::Float(value) => write!(f, "{}", value),
            Token::String(ref content) => write!(f, "\"{}\"", content),
            Token::Vector(ref content) => write!(f, "[{}]", content),
            Token::FnIdent(ref name) => write!(f, "<FnIdent {}>", name),
            Token::VecAcc(ref name) => write!(f, "<VecAcc {}>", name),
            Token::IntType => write!(f, "int"),
            Token::FloatType => write!(f, "float"),
            Token::StringType => write!(f, "string"),
            Token::BoolType => write!(f, "bool"),

            Token::EOF => write!(f, "EOF"),
            Token::Err => write!(f, "Error"),
        }
    }
}
