use crate::{error::Position, token::Token};

pub type Statements = Vec<StmtKind>;

/// 宣言
///
/// 値を返さない。
#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub(crate) kind: DeclKind,
    pub(crate) position: Position,
}

impl Decl {
    pub fn new(kind: DeclKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn position(&self) -> Position {
        self.position
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    /// 文
    Stmt(Stmt),
    /// 変数定義
    Let(Ident, Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    kind: Box<StmtKind>,
    position: Position,
}

impl Stmt {
    pub fn new(kind: impl Into<Box<StmtKind>>, position: Position) -> Self {
        Self {
            kind: kind.into(),
            position,
        }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }

    pub fn position(&self) -> Position {
        self.position
    }
}

/// 文
///
/// 文はVoid型の唯一の値voidを返す
#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    /// 空の文
    Empty,
    /// 式
    Expr(Expr),
    // While(Expr, Block),
    /// セミコロン付きの式
    SemiExpr(Expr),
}

impl StmtKind {
    pub fn includes_semi(&self) -> bool {
        matches!(self, StmtKind::SemiExpr(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub(crate) kind: Box<ExprKind>,
    pub(crate) position: Position,
}

impl Expr {
    pub fn new(kind: impl Into<Box<ExprKind>>, position: Position) -> Self {
        Self {
            kind: kind.into(),
            position,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn position(&self) -> Position {
        self.position
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    /// print
    Print(Expr),

    Block(Block),

    Number(f64),
    String(String),
    Ident(Ident),
    Bool(bool),

    Minus(Expr),

    Assign(Expr, Expr),
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Eq(Expr, Expr),
    Ne(Expr, Expr),
    Gt(Expr, Expr),
    Ge(Expr, Expr),
    Lt(Expr, Expr),
    Le(Expr, Expr),
    And(Expr, Expr),
    Or(Expr, Expr),
}

impl ExprKind {
    pub fn maybe_no_semicolon(&self) -> bool {
        matches!(self, ExprKind::Block { .. })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// ブロック内の文
    pub decls: Vec<Decl>,
    /// ブロックの最後の式
    pub return_expr: Option<Expr>,
}

impl Block {
    pub fn new(decls: Vec<Decl>, return_expr: Option<Expr>) -> Self {
        Self { decls, return_expr }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub position: Position,
}

impl Ident {
    pub fn new(name: String, position: Position) -> Self {
        Self { name, position }
    }
}
