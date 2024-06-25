use crate::token::Token;

pub type Statements = Vec<Statement>;

/// 宣言
///
/// 値を返さない。
pub enum Declaration {}

/// 文
///
/// 文はVoid型の唯一の値voidを返す
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Empty,
    Expr(Expr),
    Let(Token, Expr),
    Print(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Block(Statements),

    Number(f64),
    String(String),
    Ident(String),
    Bool(bool),

    Minus(Box<Expr>),

    Assign(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}
