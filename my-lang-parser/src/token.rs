use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    line: usize,
    column: usize,
    lexeme: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize, lexeme: String) -> Self {
        Self { kind, line, column, lexeme }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Comment,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    True,
    False,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    AndAnd,
    OrOr,
    Ident(String),
    String(String),
    Number(f64),
    Keyword(Keyword),
    Range,
    RangeInclusive,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    True,
    False,
    Let,
    Var,
    If,
    Else,
    Fn,
    Return,
    While,
    For,
    In,
    Break,
    Continue,
    Class,
}
