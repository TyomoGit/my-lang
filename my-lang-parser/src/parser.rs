use std::fmt::Display;

use crate::{
    ast::{Block, Decl, DeclKind, Expr, ExprKind, Ident, Stmt, StmtKind},
    error::{Error, ErrorKind, Position},
    token::{str_token_kind_list, Keyword, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    position: Position,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, position: Position) -> Self {
        Self { kind, position }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    NotFound {
        expected: TokenKind,
    },
    UnexpectedToken {
        token: Token,
        expected: Vec<TokenKind>,
    },
    UnexpectedEof,
}

impl std::error::Error for ParseError {}
impl Error for ParseError {
    fn position(&self) -> crate::error::Position {
        self.position
    }

    fn kind() -> crate::error::ErrorKind {
        ErrorKind::Parse
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            ParseErrorKind::NotFound { expected } => format!("Not found. Expected `{}`.", expected),
            ParseErrorKind::UnexpectedToken { token, expected } => format!(
                "Unexpected token `{}`. Expected `{}`.",
                token,
                str_token_kind_list(expected.iter())
            ),
            ParseErrorKind::UnexpectedEof => "Unexpected end of file".to_string(),
        };

        write!(f, "{}", string)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    has_error: bool,
}

macro_rules! matches_token {
    ($token_opt:expr, $pat:pat) => {
        matches!(($token_opt).map(|token| token.kind()), Some(_token @ $pat))
    };
}

// MARK: Parser
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            has_error: false,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Decl>, Vec<ParseError>> {
        self.parse_statements()
    }

    fn parse_statements(&mut self) -> Result<Vec<Decl>, Vec<ParseError>> {
        self.parse_until(None)
    }

    fn parse_until(&mut self, kind: Option<&TokenKind>) -> Result<Vec<Decl>, Vec<ParseError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        let kind = kind.map(std::mem::discriminant);

        while self.peek().is_some()
            && !(self
                .peek()
                .map(|token| std::mem::discriminant(token.kind()))
                == kind)
        {
            match self.parse_declaration() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    self.synchronize();
                    errors.extend(err);
                }
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn synchronize(&mut self) {
        while let Some(current) = self.peek() {
            if *self.previous().unwrap().kind() == TokenKind::Semicolon {
                return;
            }

            match current.kind() {
                TokenKind::LeftBrace | TokenKind::RightBrace => {
                    return;
                }
                TokenKind::Keyword(keyword) => match keyword {
                    Keyword::Class
                    | Keyword::Fn
                    | Keyword::Let
                    | Keyword::For
                    | Keyword::If
                    | Keyword::While
                    | Keyword::Print
                    | Keyword::Return => {
                        return;
                    }

                    _ => (),
                },
                _ => (),
            }

            self.advance();
        }
    }

    // MARK: Declaration

    /// parse declaration
    fn parse_declaration(&mut self) -> Result<Decl, Vec<ParseError>> {
        let token = self.advance();
        let position = token.position();

        let decl = match token.kind() {
            TokenKind::Keyword(Keyword::Let) => self.let_decl()?,
            _ => {
                self.back();
                Decl::new(DeclKind::Stmt(self.parse_statement(true)?), position)
            }
        };

        Ok(decl)
    }

    fn let_decl(&mut self) -> Result<Decl, Vec<ParseError>> {
        let position = self.previous().unwrap().position();
        let ident = self
            .consume_token(TokenKind::Ident(String::new()))
            .ok_or_else(|| {
                vec![ParseError::new(
                    ParseErrorKind::UnexpectedEof,
                    self.peek().unwrap().position(),
                )]
            })?;
        let TokenKind::Ident(ident) = ident.into_kind() else {
            unreachable!()
        };

        let _equal = self.consume_token(TokenKind::Equal).ok_or_else(|| {
            vec![ParseError::new(
                ParseErrorKind::UnexpectedEof,
                self.peek().unwrap().position(),
            )]
        })?;
        let expr = self.parse_statement(false)?;
        let _semicolon = self.consume_token(TokenKind::Semicolon).ok_or_else(|| {
            vec![ParseError::new(
                ParseErrorKind::UnexpectedEof,
                self.peek().unwrap().position(),
            )]
        })?;

        Ok(Decl::new(
            DeclKind::Let(Ident::new(ident, position), expr),
            position,
        ))
    }

    // MARK: Statement

    fn parse_statement(&mut self, can_put_semi: bool) -> Result<Stmt, Vec<ParseError>> {
        let token = self.advance();
        let (stmt, semi_check_required) = match token.kind() {
            TokenKind::Semicolon => (Stmt::new(StmtKind::Empty, token.position()), false),
            _ => {
                self.back();
                let expr = self.parse_expr()?;
                let pos = expr.position();
                let expr_stmt = if matches!(
                    self.peek().map(|token| token.kind()),
                    Some(TokenKind::Semicolon)
                ) && can_put_semi
                {
                    self.advance();
                    Stmt::new(StmtKind::SemiExpr(expr), pos)
                } else {
                    Stmt::new(StmtKind::Expr(expr), pos)
                };

                (expr_stmt, false)
            }
        };

        if semi_check_required && can_put_semi {
            let _semicolon = self.consume_token(TokenKind::Semicolon).ok_or_else(|| {
                vec![ParseError::new(
                    ParseErrorKind::UnexpectedToken {
                        token,
                        expected: vec![TokenKind::Semicolon],
                    },
                    self.previous().unwrap().position(),
                )]
            })?;
        }

        Ok(stmt)
    }

    // MARK: Expression

    fn parse_expr(&mut self) -> Result<Expr, Vec<ParseError>> {
        // if matches_token!(self.peek(), TokenKind::LeftBrace) {
        //     self.advance();
        //     self.block_expr()
        // } else {
        //     self.assignment()
        // }

        self.assignment()
    }

    fn block_expr(&mut self) -> Result<Expr, Vec<ParseError>> {
        let position = self.previous().unwrap().position();

        let block = self.parse_block()?;
        Ok(Expr::new(ExprKind::Block(block), position))
    }

    fn parse_block(&mut self) -> Result<Block, Vec<ParseError>> {
        // empty block
        if let Some(_right_brace) = self.consume_token(TokenKind::RightBrace) {
            self.advance();
            let ret = Block::new(Vec::new(), None);

            return Ok(ret);
        }

        let mut decls = self.parse_until(Some(&TokenKind::RightBrace))?;
        let _right_brace = self.consume_token(TokenKind::RightBrace).ok_or_else(|| {
            vec![ParseError::new(
                ParseErrorKind::UnexpectedEof,
                self.peek().unwrap().position(),
            )]
        })?;

        let return_expr = if matches!(
            decls.last().map(|decl| decl.kind()),
            Some(DeclKind::Stmt(stmt)) if matches!(stmt.kind(), StmtKind::Expr(_))
        ) {
            let last = decls.pop().unwrap();
            let DeclKind::Stmt(lst) = last.kind() else {
                unreachable!();
            };
            let StmtKind::Expr(expr) = lst.kind() else {
                unreachable!();
            };

            Some(expr.clone())
        } else {
            None
        };

        let invalid_exprs: Vec<_> = decls
            .iter()
            .filter(|stmt| matches!(stmt.kind(), DeclKind::Stmt(stmt) if matches!(stmt.kind(), StmtKind::Expr(_))))
            .map(|decl| match decl.kind() {
                DeclKind::Stmt(stmt) => match stmt.kind() {
                    StmtKind::Expr(expr) => ParseError::new(
                        ParseErrorKind::NotFound {
                            expected: TokenKind::Semicolon,
                        },
                        expr.position(),
                    ),
                    _ => unreachable!(),
                }
                _ => unreachable!(),
            })
            .collect();

        if !invalid_exprs.is_empty() {
            return Err(invalid_exprs);
        }

        Ok(Block { decls, return_expr })
    }

    fn assignment(&mut self) -> Result<Expr, Vec<ParseError>> {
        let expr = self.logical_or()?;
        let position = expr.position();

        if matches_token!(self.peek(), TokenKind::Equal) {
            let _token = self.advance();
            let right = self.assignment()?;
            Ok(Expr::new(ExprKind::Assign(expr, right), position))
        } else {
            Ok(expr)
        }
    }

    fn logical_or(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.logical_and()?;
        let position = expr.position();

        while matches_token!(self.peek(), TokenKind::OrOr) {
            let token = self.advance();
            let right = self.logical_and()?;
            expr = match token.kind() {
                TokenKind::OrOr => Expr::new(ExprKind::Or(expr, right), position),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.equality()?;
        let position = expr.position();

        while matches_token!(self.peek(), TokenKind::AndAnd) {
            let token = self.advance();
            let right = self.equality()?;
            expr = match token.kind() {
                TokenKind::AndAnd => Expr::new(ExprKind::And(expr, right), position),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.comparison()?;
        let position = expr.position();

        while matches_token!(self.peek(), TokenKind::EqualEqual | TokenKind::BangEqual) {
            let token = self.advance();
            let right = self.comparison()?;
            expr = match token.kind() {
                TokenKind::EqualEqual => Expr::new(ExprKind::Eq(expr, right), position),
                TokenKind::BangEqual => Expr::new(ExprKind::Ne(expr, right), position),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.term()?;
        let position = expr.position();

        while matches_token!(
            self.peek(),
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let token = self.advance();
            let right = self.term()?;
            let kind = match token.kind() {
                TokenKind::Greater => ExprKind::Gt(expr, right),
                TokenKind::GreaterEqual => ExprKind::Ge(expr, right),
                TokenKind::Less => ExprKind::Lt(expr, right),
                TokenKind::LessEqual => ExprKind::Le(expr, right),
                _ => unreachable!(),
            };

            expr = Expr::new(kind, position);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.factor()?;
        let position = expr.position();

        while matches_token!(self.peek(), TokenKind::Plus | TokenKind::Minus) {
            let token = self.advance();
            let right = self.factor()?;
            let kind = match token.kind() {
                TokenKind::Plus => ExprKind::Add(expr, right),
                TokenKind::Minus => ExprKind::Sub(expr, right),
                _ => unreachable!(),
            };

            expr = Expr::new(kind, position);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.unary()?;
        let position = expr.position();

        while matches_token!(self.peek(), TokenKind::Star | TokenKind::Slash) {
            let token = self.advance();
            let right = self.unary()?;
            let kind = match token.kind() {
                TokenKind::Star => ExprKind::Mul(expr, right),
                TokenKind::Slash => ExprKind::Div(expr, right),
                _ => unreachable!(),
            };

            expr = Expr::new(kind, position);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Vec<ParseError>> {
        let Some(token) = self.peek().cloned() else {
            let token = self.tokens.last().unwrap();
            let err = ParseError::new(ParseErrorKind::UnexpectedEof, token.position());
            return Err(vec![err]);
        };

        match token.kind() {
            TokenKind::Minus => {
                self.advance();
                let right = self.unary()?;
                Ok(Expr::new(ExprKind::Minus(right), token.position()))
            }
            _ => self.block(),
        }
    }

    fn block(&mut self) -> Result<Expr, Vec<ParseError>> {
        if matches_token!(self.peek(), TokenKind::LeftBrace) {
            self.advance();
            self.block_expr()
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, Vec<ParseError>> {
        if matches_token!(self.peek(), TokenKind::Keyword(Keyword::Print)) {
            self.advance();
            self.print_expr()
        } else {
            self.primary()
        }

        // TODO: function call
    }

    fn primary(&mut self) -> Result<Expr, Vec<ParseError>> {
        let token = self.advance();
        let position = token.position();

        let kind = match token.kind() {
            TokenKind::Number(number) => ExprKind::Number(*number),
            TokenKind::String(string) => ExprKind::String(string.clone()),
            TokenKind::Ident(ident) => ExprKind::Ident(Ident::new(ident.clone(), position)),
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::True => ExprKind::Bool(true),
                Keyword::False => ExprKind::Bool(false),
                Keyword::If => *self.if_expr()?.kind,
                _ => {
                    let err = ParseError::new(
                        ParseErrorKind::UnexpectedToken {
                            token: token.clone(),
                            expected: vec![
                                TokenKind::Keyword(Keyword::True),
                                TokenKind::Keyword(Keyword::False),
                            ],
                        },
                        position,
                    );
                    return Err(vec![err]);
                }
            },
            TokenKind::LeftParen => {
                let Expr { kind, .. } = self.parse_expr()?;
                if self.consume_token(TokenKind::RightParen).is_some() {
                    *kind
                } else {
                    let err = ParseError::new(
                        ParseErrorKind::UnexpectedToken {
                            token: token.clone(),
                            expected: vec![TokenKind::RightParen],
                        },
                        position,
                    );
                    return Err(vec![err]);
                }
            }
            _ => {
                let err = ParseError::new(
                    ParseErrorKind::UnexpectedToken {
                        token: token.clone(),
                        expected: vec![
                            TokenKind::Number(0.0),
                            TokenKind::String("".to_string()),
                            TokenKind::Keyword(Keyword::True),
                            TokenKind::Keyword(Keyword::False),
                            TokenKind::LeftParen,
                        ],
                    },
                    position,
                );
                return Err(vec![err]);
            }
        };

        Ok(Expr::new(kind, position))
    }

    fn print_expr(&mut self) -> Result<Expr, Vec<ParseError>> {
        let position = self.previous().unwrap().position();

        let expr = self.parse_expr()?;

        Ok(Expr::new(ExprKind::Print(expr), position))
    }

    fn if_expr(&mut self) -> Result<Expr, Vec<ParseError>> {
        let position = self.previous().unwrap().position();

        let cond = self.parse_expr()?;

        if self.consume_token(TokenKind::LeftBrace).is_none() {
            let err = ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    token: self.previous().unwrap().clone(),
                    expected: vec![TokenKind::LeftBrace],
                },
                position,
            );
            return Err(vec![err]);
        }
        let then_block = self.parse_block()?;

        if self
            .consume_token(TokenKind::Keyword(Keyword::Else))
            .is_none()
        {
            return Ok(Expr::new(
                ExprKind::If {
                    cond,
                    then_block,
                    else_block: None,
                },
                position,
            ));
        }

        if self
            .consume_token(TokenKind::Keyword(Keyword::If))
            .is_some()
        {
            // else if
            let if_expr = self.if_expr()?;
            let block = Block::new(
                vec![Decl::new(
                    DeclKind::Stmt(Stmt::new(StmtKind::Expr(if_expr), position)),
                    position,
                )],
                None,
            );

            Ok(Expr::new(
                ExprKind::If {
                    cond,
                    then_block,
                    else_block: Some(block),
                },
                position,
            ))
        } else {
            // else
            if self.consume_token(TokenKind::LeftBrace).is_none() {
                let err = ParseError::new(
                    ParseErrorKind::UnexpectedToken {
                        token: self.previous().unwrap().clone(),
                        expected: vec![TokenKind::LeftBrace],
                    },
                    position,
                );
                return Err(vec![err]);
            }

            let else_block = self.parse_block()?;
            Ok(Expr::new(
                ExprKind::If {
                    cond,
                    then_block,
                    else_block: Some(else_block),
                },
                position,
            ))
        }
    }

    // fn match_token(&mut self, kind: TokenKind) -> bool {
    //     if let Some(token) = self.peek() {
    //         if *token.kind() == kind {
    //             self.advance();
    //             return true;
    //         }
    //     }
    //     false
    // }

    fn advance(&mut self) -> Token {
        let token = self.tokens.get(self.current).cloned().unwrap();
        self.current += 1;
        token
    }

    fn consume_token(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.peek() {
            if std::mem::discriminant(token.kind()) == std::mem::discriminant(&kind) {
                return Some(self.advance());
            }
        }
        None
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn back(&mut self) {
        self.current -= 1;
    }
}
