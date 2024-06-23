use crate::token::{Token, TokenKind};

pub type Result<T> = std::result::Result<T, ScanError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScanError {
    kind: ScanErrorKind,
    line: usize,
    column: usize,
}

impl ScanError {
    pub fn new(kind: ScanErrorKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScanErrorKind {
    UnexpectedChar {
        found: char,
        expected: char,
    },
    UnexpectedEof,
}


pub struct Scanner {
    source: Vec<char>,

    current: usize,
    start: usize,

    line: usize,
    column: usize,
}

impl Scanner {
    pub fn new(source: Vec<char>) -> Self {
        Self { source, current: 0, start: 0, line: 1, column: 1 }
    }

    pub fn scan_tokens(&mut self) -> std::result::Result<Vec<Token>, Vec<ScanError>> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut errors: Vec<ScanError> = Vec::new();
        while self.peek().is_some() {
            self.skip_whitespace();
            match self.scan_token() {
                Ok(token) => tokens.push(token),
                Err(err) => errors.push(err),
            }
            self.skip_whitespace();
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    /// トークンを1つスキャンする
    fn scan_token(&mut self) -> Result<Token> {
        self.start = self.current;

        let c = self.advance().unwrap();

        if is_alpha(c) {
            return self.identifier();
        }

        if is_digit(c) {
            return self.number();
        }

        let token = match c {
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            ';' => self.make_token(TokenKind::Semicolon),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Dot),
            '-' => self.make_token(TokenKind::Minus),
            '+' => self.make_token(TokenKind::Plus),
            '*' => self.make_token(TokenKind::Star),
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::BangEqual)
                } else {
                    self.make_token(TokenKind::Bang)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqualEqual)
                } else {
                    self.make_token(TokenKind::Equal)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::LessEqual)
                } else {
                    self.make_token(TokenKind::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::GreaterEqual)
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != Some(&'\n') && self.peek().is_some() {
                        self.advance();
                    }
                    self.make_token(TokenKind::Comment)
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.advance();
                    self.make_token(TokenKind::AndAnd)
                } else {
                    self.make_token(TokenKind::And)
                }
            },

            '|' => {
                if self.match_char('|') {
                    self.advance();
                    self.make_token(TokenKind::OrOr)
                } else {
                    self.make_token(TokenKind::Or)
                }
            },
            _ => return Err(self.make_error(ScanErrorKind::UnexpectedEof)),
        };

        Ok(token)
    }

    fn identifier(&mut self) -> Result<Token> {
        while self.peek().is_some() && is_alpha(self.peek().unwrap()) {
            self.advance();
        }
        let ident = self.collect_string().unwrap();

        let kind = match ident.as_str() {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(ident),
        };

        Ok(self.make_token(kind))
    }

    fn number(&mut self) -> Result<Token> {
        while self.peek().is_some() && is_digit(self.peek().unwrap()) {
            self.advance();
        }
        Ok(self.make_token(TokenKind::Number(
            self.collect_string().unwrap().parse().unwrap(),
        )))
    }

    fn collect_string(&self) -> Option<String> {
        self.source.get(self.start..self.current).map(|s| s.iter().collect())
    }

    fn advance(&mut self) -> Option<&char> {
        let result = self.source.get(self.current);
        self.current += 1;
        result
    }

    fn peek(&self) -> Option<&char> {
        self.source.get(self.current)
    }

    fn peek_next(&self) -> Option<&char> {
        self.source.get(self.current + 1)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.line, self.column, self.collect_string().unwrap())
    }

    fn make_error(&self, kind: ScanErrorKind) -> ScanError {
        ScanError::new(kind, self.line, self.column)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(&expected) {
            self.current += 1;
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some() && self.peek().unwrap().is_whitespace() {
            if self.peek() == Some(&'\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.advance();
        }
    }
}

fn is_digit(c: &char) -> bool {
    c.is_ascii_digit()
}

fn is_alpha(c: &char) -> bool {
    c.is_alphabetic()
}
