use std::fmt::Display;

use crate::{
    ast::{Expr, Statement},
    error::{Error, Position},
    token::{str_token_kind_list, Keyword, Token, TokenKind},
};

type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    position: Position,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
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
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            ParseErrorKind::UnexpectedToken { token, expected } => format!("Unexpected token `{}`. Expected `{}`.", token, str_token_kind_list(expected.iter())),
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

    pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<ParseError>> {
        self.parse_statements()
    }

    fn parse_statements(&mut self) -> Result<Vec<Statement>, Vec<ParseError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        while self.peek().is_some() {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => errors.extend(err),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, Vec<ParseError>> {
        // let Some(token) = self.advance() else {
        //     return Err(vec![self.make_error(
        //         ParseErrorKind::UnexpectedEof,
        //         self.tokens.last().unwrap(),
        //     )]);
        // };
        let token = self.advance();
        let stmt = match token.kind() {
            TokenKind::Semicolon => Statement::Empty,
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::Let => self.let_statement()?,
                Keyword::Print => self.print_statement()?,
                _ => {
                    let expected = [Keyword::Let, Keyword::Print]
                        .iter()
                        .map(|k| TokenKind::Keyword(k.clone()));
                    return Err(vec![self.make_error(
                        ParseErrorKind::UnexpectedToken{ token: token.clone(), expected: expected.collect() },
                        &token,
                    )])
                }
            },
            _ => {
                self.back();
                Statement::Expr(self.parse_expr()?)
            }
        };

        Ok(stmt)
    }

    fn let_statement(&mut self) -> Result<Statement, Vec<ParseError>> {
        let ident: Option<Token> = self.consume_token(TokenKind::Ident(String::new()));
        let equal: Option<Token> = self.consume_token(TokenKind::Equal);
        let expr: Result<Expr, Vec<ParseError>> = self.parse_expr();
        let semicolon: Option<Token> = self.consume_token(TokenKind::Semicolon);
        let (Some(ident), Some(_equal), Ok(expr), Some(_semicolon)) =
            (ident, equal, expr, semicolon)
        else {
            panic!()
        };

        Ok(Statement::Let(ident, expr))
    }

    fn print_statement(&mut self) -> Result<Statement, Vec<ParseError>> {
        let expr = self.parse_expr()?;
        let semicolon = self.consume_token(TokenKind::Semicolon);
        let Some(_semicolon) = semicolon else {
            return Err(vec![self.make_error(
                ParseErrorKind::UnexpectedToken{ token: self.peek().unwrap().clone(), expected: vec![TokenKind::Semicolon] },
                self.peek().unwrap(),
            )]);
        };
        Ok(Statement::Print(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, Vec<ParseError>> {
        if match_token!(self.peek(), TokenKind::LeftBrace) {
            self.advance();
            self.block()
        } else {
            self.assignment()
        }
    }

    fn block(&mut self) -> Result<Expr, Vec<ParseError>> {
        todo!()
    }

    fn assignment(&mut self) -> Result<Expr, Vec<ParseError>> {
        let expr = self.logical_or()?;

        if match_token!(self.peek(), TokenKind::Equal) {
            let _token = self.advance();
            let right = self.assignment()?;
            Ok(Expr::Assign(expr.into(), right.into()))
        } else {
            Ok(expr)
        }
    }

    fn logical_or(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.logical_and()?;

        while match_token!(self.peek(), TokenKind::OrOr) {
            let token = self.advance();
            let right = self.logical_and()?;
            expr = match token.kind() {
                TokenKind::OrOr => Expr::Or(expr.into(), right.into()),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.equality()?;

        while match_token!(self.peek(), TokenKind::AndAnd) {
            let token = self.advance();
            let right = self.equality()?;
            expr = match token.kind() {
                TokenKind::AndAnd => Expr::And(expr.into(), right.into()),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.comparison()?;

        while match_token!(self.peek(), TokenKind::EqualEqual | TokenKind::BangEqual) {
            let token = self.advance();
            let right = self.comparison()?;
            expr = match token.kind() {
                TokenKind::EqualEqual => Expr::Eq(expr.into(), right.into()),
                TokenKind::BangEqual => Expr::Ne(expr.into(), right.into()),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.term()?;

        while match_token!(
            self.peek(),
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let token = self.advance();
            let right = self.term()?;
            expr = match token.kind() {
                TokenKind::Greater => Expr::Gt(expr.into(), right.into()),
                TokenKind::GreaterEqual => Expr::Ge(expr.into(), right.into()),
                TokenKind::Less => Expr::Lt(expr.into(), right.into()),
                TokenKind::LessEqual => Expr::Le(expr.into(), right.into()),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.factor()?;
        while match_token!(self.peek(), TokenKind::Plus | TokenKind::Minus) {
            let token = self.advance();
            let right = self.factor()?;
            expr = match token.kind() {
                TokenKind::Plus => Expr::Add(expr.into(), right.into()),
                TokenKind::Minus => Expr::Sub(expr.into(), right.into()),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Vec<ParseError>> {
        let mut expr = self.unary()?;
        while match_token!(self.peek(), TokenKind::Star | TokenKind::Slash) {
            let token = self.advance();
            let right = self.unary()?;
            expr = match token.kind() {
                TokenKind::Star => Expr::Mul(expr.into(), right.into()),
                TokenKind::Slash => Expr::Div(expr.into(), right.into()),
                _ => unreachable!(),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Vec<ParseError>> {
        let Some(token) = self.peek() else {
            let token = self.tokens.last().unwrap();
            let err = self.make_error(ParseErrorKind::UnexpectedEof, token);
            return Err(vec![err]);
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

    fn primary(&mut self) -> Result<Expr, Vec<ParseError>> {
        let token = self.advance();

        let expr = match token.kind() {
            TokenKind::Number(number) => Expr::Number(*number),
            TokenKind::String(string) => Expr::String(string.clone()),
            TokenKind::Ident(ident) => Expr::Ident(ident.clone()),
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::True => Expr::Bool(true),
                Keyword::False => Expr::Bool(false),
                _ => {
                    let err =
                        self.make_error(ParseErrorKind::UnexpectedToken{ token: token.clone(), expected: vec![TokenKind::Keyword(Keyword::True), TokenKind::Keyword(Keyword::False)] }, &token);
                    return Err(vec![err]);
                }
            },
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;
                if match_token!(self.peek(), TokenKind::RightParen) {
                    self.advance();
                    expr
                } else {
                    let err =
                        self.make_error(ParseErrorKind::UnexpectedToken{ token: token.clone(), expected: vec![TokenKind::RightParen] }, &token);
                    return Err(vec![err]);
                }
            }
            _ => {
                let err = self.make_error(ParseErrorKind::UnexpectedToken{ token: token.clone(), expected: vec![TokenKind::Number(0.0), TokenKind::String("".to_string()), TokenKind::Keyword(Keyword::True), TokenKind::Keyword(Keyword::False), TokenKind::LeftParen] }, &token);
                return Err(vec![err]);
            }
        };

        Ok(expr)
    }

    fn make_error(&self, kind: ParseErrorKind, token: &Token) -> ParseError {
        ParseError {
            kind,
            position: token.position(),
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

    fn back(&mut self) {
        self.current -= 1;
    }
}
