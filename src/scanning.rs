use std::{iter::Peekable, str::Chars};

pub fn scan_tokens(input: &str) -> Result<Vec<Token>, Vec<ScanError>> {
    let scanner = Scanner::new(input);
    scanner.scan()
}

struct Scanner<'a> {
    input: Peekable<Chars<'a>>,
    col: usize,
    line: usize,
    tokens: Vec<Token>,
    errors: Vec<ScanError>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &str) -> Scanner {
        Scanner {
            input: input.chars().into_iter().peekable(),
            col: 0,
            line: 1,
            tokens: vec![],
            errors: vec![],
        }
    }

    fn add_single_ch_token(&mut self, token: TokenKind) {
        self.tokens.push(Token {
            kind: token,
            start_span: (self.line, self.col),
            end_span: (self.line, self.col + 1),
        });
        self.col += 1;
    }

    fn add_conditional_token(&mut self, short: TokenKind, long: TokenKind, second_ch: char) {
        if self.input.next_if_eq(&second_ch).is_some() {
            self.tokens.push(Token {
                kind: long,
                start_span: (self.line, self.col),
                end_span: (self.line, self.col + 2),
            });
            self.col += 2;
        } else {
            self.add_single_ch_token(short);
        }
    }

    fn consume_until(&mut self, matching_ch: char) {
        while let Some(ch) = self.input.next() {
            if ch == '\n' {
                self.col = 0;
                self.line += 1;
            } else {
                self.col += 1;
            }
            if matching_ch == ch {
                break;
            }
        }
    }

    fn consume_line(&mut self) {
        self.consume_until('\n')
    }

    fn add_string_literal(&mut self) {
        let mut literal = String::new();
        let start_line = self.line;
        let start_col = self.col;
        loop {
            match self.input.next() {
                Some(ch) => {
                    if ch == '\n' {
                        self.col = 0;
                        self.line += 1;
                        literal.push(ch);
                    } else if ch == '"' {
                        self.col += 1;
                        self.tokens.push(Token {
                            kind: TokenKind::String(literal),
                            start_span: (start_line, start_col),
                            end_span: (self.line, self.col),
                        });
                        break;
                    } else {
                        self.col += 1;
                        literal.push(ch);
                    }
                }
                None => {
                    self.errors.push(ScanError::new(
                        self.line,
                        self.col,
                        String::from("Unterminated string literal"),
                    ));
                    break;
                }
            }
        }
    }

    fn add_numeric_literal(&mut self, ch: char) {
        let mut literal = String::from(ch);
        let start_line = self.line;
        let start_col = self.col - 1;
        loop {
            match self.input.peek() {
                Some(ch) => {
                    if ch.is_digit(10) || *ch == '.' {
                        self.col += 1;
                        literal.push(*ch);
                    } else {
                        break;
                    }
                }
                None => break,
            }
            // todo: next_if_eq might be useful here
            let _ = self.input.next();
        }
        if let Ok(num) = literal.parse::<f64>() {
            self.tokens.push(Token {
                kind: TokenKind::Number(num),
                start_span: (start_line, start_col),
                end_span: (self.line, self.col),
            });
        } else {
            self.errors.push(ScanError::new(
                start_line,
                start_col,
                String::from("Invalid numeric literal"),
            ));
        }
    }

    fn add_ident(&mut self, ch: char) {
        let mut ident = String::from(ch);
        let start_col = self.col;
        loop {
            match self.input.peek() {
                Some(ch) => {
                    if ch.is_alphanumeric() || *ch == '_' {
                        self.col += 1;
                        ident.push(*ch);
                    } else {
                        break;
                    }
                }
                None => break,
            }
            let _ = self.input.next();
        }

        let token = match ident.as_str() {
            "and" => TokenKind::And,
            "class" => TokenKind::Class,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "if" => TokenKind::If,
            "nil" => TokenKind::Nil,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "super" => TokenKind::Super,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            _ => TokenKind::Ident(ident),
        };
        self.tokens.push(Token {
            kind: token,
            start_span: (self.line, start_col),
            end_span: (self.line, self.col),
        });
    }

    pub fn scan(mut self) -> Result<Vec<Token>, Vec<ScanError>> {
        while let Some(ch) = self.input.next() {
            match ch {
                '\n' => {
                    let _ = (self.col = 0, self.line += 1);
                }
                '\0' => break,
                ' ' | '\r' | '\t' => self.col += 1,
                '(' => self.add_single_ch_token(TokenKind::LeftParen),
                ')' => self.add_single_ch_token(TokenKind::RightParen),
                '{' => self.add_single_ch_token(TokenKind::LeftBrace),
                '}' => self.add_single_ch_token(TokenKind::RightBrace),
                ',' => self.add_single_ch_token(TokenKind::Comma),
                '.' => self.add_single_ch_token(TokenKind::Dot),
                '-' => self.add_single_ch_token(TokenKind::Minus),
                '+' => self.add_single_ch_token(TokenKind::Plus),
                ';' => self.add_single_ch_token(TokenKind::Semicolon),
                '*' => self.add_single_ch_token(TokenKind::Star),
                '!' => self.add_conditional_token(TokenKind::Bang, TokenKind::BangEqual, '='),
                '=' => self.add_conditional_token(TokenKind::Equals, TokenKind::EqualEquals, '='),
                '<' => self.add_conditional_token(TokenKind::Less, TokenKind::LessEqual, '='),
                '>' => self.add_conditional_token(TokenKind::Greater, TokenKind::GreaterEqual, '='),
                '/' => {
                    if self.input.next_if_eq(&'/').is_some() {
                        self.consume_line();
                    } else {
                        self.add_single_ch_token(TokenKind::Slash);
                    }
                }
                '"' => self.add_string_literal(),
                ch => {
                    if ch.is_digit(10) {
                        self.add_numeric_literal(ch);
                        continue;
                    }
                    if ch.is_alphabetic() || ch == '_' {
                        self.add_ident(ch);
                        continue;
                    }
                    // If we get here, we aren't expecting the ch
                    self.errors.push(ScanError::new(
                        self.line,
                        self.col,
                        format!("Unexpected character: {ch}"),
                    ));
                    self.col += 1;
                }
            }
        }

        Ok(self.tokens)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    /// The line number and column where the token begins
    pub start_span: (usize, usize),
    /// The line number and column where the token ends
    pub end_span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equals,
    EqualEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Ident(String),
    String(String),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

impl TokenKind {
    pub fn is_eof(&self) -> bool {
        matches!(*self, TokenKind::Eof)
    }
}

#[derive(Debug, Clone)]
pub struct ScanError {
    error_txt: String,
}

impl ScanError {
    pub fn new(line: usize, col: usize, error: String) -> Self {
        Self {
            error_txt: format!("[{line}:{col}] Scan error: {error}"),
        }
    }
}

impl std::fmt::Display for ScanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error_txt.fmt(f)
    }
}

impl std::error::Error for ScanError {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }

    fn description(&self) -> &str {
        &self.error_txt
    }

    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

#[cfg(test)]
mod test {
    use crate::scanning::{scan_tokens, TokenKind};

    #[test]
    fn test_var_assign() {
        let input_text = "var language = \"lox\";";
        let tokens = scan_tokens(&input_text)
            .unwrap()
            .into_iter()
            .map(|token| token.kind)
            .collect::<Vec<_>>();
        assert_eq!(TokenKind::Var, tokens[0]);
        assert_eq!(TokenKind::Ident(String::from("language")), tokens[1]);
        assert_eq!(TokenKind::Equals, tokens[2]);
        assert_eq!(TokenKind::String(String::from("lox")), tokens[3]);
        assert_eq!(TokenKind::Semicolon, tokens[4]);
    }

    #[test]
    fn test_literals() {
        let input = "-5";
        let tokens = scan_tokens(&input).unwrap();
        assert_eq!(TokenKind::Minus, tokens[0].kind);
        assert_eq!(TokenKind::Number(5.), tokens[1].kind);
    }
}
