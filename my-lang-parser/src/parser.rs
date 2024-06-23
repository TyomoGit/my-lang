use std::{error::Error, fmt::Display};

use crate::{ast::Expr, token::{Token, TokenKind}};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    line: usize,
    column: usize,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken(Token),
    UnexpectedEof,
}

impl Error for ParseError {}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.line, self.column, self.kind)
    }
}
impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            ParseErrorKind::UnexpectedToken(token) => format!("Unexpected token: {}", token),
            ParseErrorKind::UnexpectedEof => "Unexpected end of file".to_string(),
            
        };

        write!(f, "{}", string)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

macro_rules! match_token {
    ($token_opt:expr, $pat:pat) => {
        matches!(($token_opt).map(|token| token.kind()), Some(_token @ $pat))
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> std::result::Result<Expr, ParseErrorKind> {
        self.parse_expr().map_err(|error| error.kind)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.logical_or()?;

        if match_token!(self.peek(), TokenKind::Equal) {
            let _token = self.advance();
            let right = self.assignment()?;
            Ok(Expr::Assign(expr.into(), right.into()))
        } else {
            Ok(expr)
        }
    }

    fn logical_or(&mut self) -> Result<Expr> {
        let mut expr = self.logical_and()?;

        while match_token!(self.peek(), TokenKind::OrOr) {
            let token = self.advance();
            let right = self.logical_and()?;
            expr = match token.kind() {
                TokenKind::OrOr => Expr::Or(expr.into(), right.into()),
                _ => unreachable!()
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while match_token!(self.peek(), TokenKind::AndAnd) {
            let token = self.advance();
            let right = self.equality()?;
            expr = match token.kind() {
                TokenKind::AndAnd => Expr::And(expr.into(), right.into()),
                _ => unreachable!()
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while match_token!(self.peek(), TokenKind::EqualEqual | TokenKind::BangEqual) {
            let token = self.advance();
            let right = self.comparison()?;
            expr = match token.kind() {
                TokenKind::EqualEqual => Expr::Eq(expr.into(), right.into()),
                TokenKind::BangEqual => Expr::Ne(expr.into(), right.into()),
                _ => unreachable!()
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while match_token!(self.peek(), TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual) {
            let token = self.advance();
            let right = self.term()?;
            expr = match token.kind() {
                TokenKind::Greater => Expr::Gt(expr.into(), right.into()),
                TokenKind::GreaterEqual => Expr::Ge(expr.into(), right.into()),
                TokenKind::Less => Expr::Lt(expr.into(), right.into()),
                TokenKind::LessEqual => Expr::Le(expr.into(), right.into()),
                _ => unreachable!()
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        while match_token!(self.peek(), TokenKind::Plus | TokenKind::Minus) {
            let token = self.advance();
            let right = self.factor()?;
            expr = match token.kind() {
                TokenKind::Plus => Expr::Add(expr.into(), right.into()),
                TokenKind::Minus => Expr::Sub(expr.into(), right.into()),
                _ => unreachable!()
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        while match_token!(self.peek(), TokenKind::Star | TokenKind::Slash) {
            let token = self.advance();
            let right = self.unary()?;
            expr = match token.kind() {
                TokenKind::Star => Expr::Mul(expr.into(), right.into()),
                TokenKind::Slash => Expr::Div(expr.into(), right.into()),
                _ => unreachable!()
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        let Some(token) = self.peek() else {
            let token = self.tokens.last().unwrap();
            return Err(self.make_error(ParseErrorKind::UnexpectedEof, token));
        };
        match token.kind() {
            TokenKind::Minus => {
                self.advance();
                let right = self.unary()?;
                Ok(Expr::Minus(right.into()))
            }
            _ => self.primary(),
        }

    }

    fn primary(&mut self) -> Result<Expr> {
        let token = self.advance();

        let expr = match token.kind() {
            TokenKind::Number(number) => Expr::Number(*number),
            TokenKind::True => Expr::Bool(true),
            TokenKind::False => Expr::Bool(false),
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if match_token!(self.peek(), TokenKind::RightParen) {
                    self.advance();
                    expr
                } else {
                    return Err(self.make_error(ParseErrorKind::UnexpectedToken(token.clone()), &token));
                }
            }
            _ => return Err(self.make_error(ParseErrorKind::UnexpectedToken(token.clone()), &token)),
        };

        Ok(expr)
    }

    fn make_error(&self, kind: ParseErrorKind, token: &Token) -> ParseError {
        ParseError {
            kind,
            line: token.line(),
            column: token.column(),
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
        let token = self.tokens[self.current].clone();
        self.current += 1;
        token
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }
}
