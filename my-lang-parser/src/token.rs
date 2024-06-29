use std::{borrow::Borrow, fmt::Display};

use crate::error::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    position: Position,
    lexeme: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl Token {
    pub fn new(kind: TokenKind, position: Position, lexeme: String) -> Self {
        Self {
            kind,
            position,
            lexeme,
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn into_kind(self) -> TokenKind {
        self.kind
    }

    pub fn position(&self) -> Position {
        self.position
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

pub fn str_token_kind_list<I: Borrow<TokenKind>>(list: impl Iterator<Item = I>) -> String {
    list.map(|kind| kind.borrow().to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenKind::Comment => "//",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Equal => "=",
            TokenKind::EqualEqual => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEqual => "!=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::And => "&",
            TokenKind::Or => "|",
            TokenKind::AndAnd => "&&",
            TokenKind::OrOr => "||",
            TokenKind::Ident(_) => "Identifier",
            TokenKind::String(_) => "String",
            TokenKind::Number(_) => "Number",
            TokenKind::Keyword(keyword) => {
                return write!(f, "{}", keyword);
            }
            TokenKind::Range => "..",
            TokenKind::RangeInclusive => "..=",
            TokenKind::Eof => "EOF",
        };

        write!(f, "{}", string)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    True,
    False,
    Let,
    Print,
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

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Let => "let",
            Keyword::Print => "print",
            Keyword::Var => "var",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Fn => "fn",
            Keyword::Return => "return",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::In => "in",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Class => "class",
        };
        write!(f, "{}", string)
    }
}
