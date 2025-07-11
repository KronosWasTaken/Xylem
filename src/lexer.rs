use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Fn,
    If,
    Else,
    While,
    Return,
    Elif,
    // Identifiers and literals
    Identifier(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    StrLiteral(String),
    FloatLiteral(f64),
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    EqEq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Not,
    // Punctuation
    Colon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    // End of file
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithLine {
    pub token: Token,
    pub line: usize,
}

pub struct Lexer<'a> {
    chars: Chars<'a>,
    current_char: Option<char>,
    pub line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current_char = chars.next();
        Lexer {
            chars,
            current_char,
            line: 1,
        }
    }

    fn advance(&mut self) {
        if self.current_char == Some('\n') {
            self.line += 1;
        }
        self.current_char = self.chars.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(c) = self.current_char {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    fn read_number(&mut self) -> i64 {
        let mut num = String::new();
        while let Some(c) = self.current_char {
            if c.is_ascii_digit() {
                num.push(c);
                self.advance();
            } else {
                break;
            }
        }
        num.parse().unwrap_or(0)
    }

    fn read_string(&mut self) -> String {
        let mut s = String::new();
        self.advance(); // skip opening quote
        while let Some(c) = self.current_char {
            if c == '"' {
                self.advance();
                break;
            } else {
                s.push(c);
                self.advance();
            }
        }
        s
    }

    pub fn next_token(&mut self) -> TokenWithLine {
        let line = self.line;
        let token = loop {
            self.skip_whitespace();
            match self.current_char {
                Some('#') => {
                    // Skip comment
                    while let Some(c) = self.current_char {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                    self.advance();
                    continue;
                }
                Some('/') => {
                    self.advance();
                    if self.current_char == Some('/') {
                        // Skip '//' comment
                        while let Some(c) = self.current_char {
                            if c == '\n' {
                                break;
                            }
                            self.advance();
                        }
                        self.advance();
                        continue;
                    } else {
                        break Token::Slash;
                    }
                }
                Some(':') => { self.advance(); break Token::Colon; }
                Some(',') => { self.advance(); break Token::Comma; }
                Some('(') => { self.advance(); break Token::LParen; }
                Some(')') => { self.advance(); break Token::RParen; }
                Some('{') => { self.advance(); break Token::LBrace; }
                Some('}') => { self.advance(); break Token::RBrace; }
                Some(';') => { self.advance(); break Token::Semicolon; }
                Some('+') => { self.advance(); break Token::Plus; }
                Some('-') => { self.advance(); break Token::Minus; }
                Some('*') => { self.advance(); break Token::Star; }
                Some('=') => {
                    self.advance();
                    if self.current_char == Some('=') { self.advance(); break Token::EqEq } else { break Token::Eq }
                }
                Some('!') => {
                    self.advance();
                    if self.current_char == Some('=') { self.advance(); break Token::NotEq } else { break Token::Not }
                }
                Some('<') => {
                    self.advance();
                    if self.current_char == Some('=') { self.advance(); break Token::Le } else { break Token::Lt }
                }
                Some('>') => {
                    self.advance();
                    if self.current_char == Some('=') { self.advance(); break Token::Ge } else { break Token::Gt }
                }
                Some('"') => { let s = self.read_string(); break Token::StrLiteral(s) }
                Some(c) if c.is_ascii_digit() => {
                    let mut num = String::new();
                    let mut is_float = false;
                    while let Some(c) = self.current_char {
                        if c.is_ascii_digit() {
                            num.push(c);
                            self.advance();
                        } else if c == '.' {
                            if is_float { break; } // Only one dot allowed
                            is_float = true;
                            num.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if is_float {
                        break Token::FloatLiteral(num.parse().unwrap_or(0.0));
                    } else {
                        break Token::IntLiteral(num.parse().unwrap_or(0));
                    }
                }
                Some(c) if c.is_alphabetic() || c == '_' => {
                    let ident = self.read_identifier();
                    break match ident.as_str() {
                        "fn" => Token::Fn,
                        "if" => Token::If,
                        "elif" => Token::Elif,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "return" => Token::Return,
                        "true" => Token::BoolLiteral(true),
                        "false" => Token::BoolLiteral(false),
                        "and" => Token::And,
                        "or" => Token::Or,
                        "not" => Token::Not,
                        _ => Token::Identifier(ident),
                    };
                }
                Some(_) => { self.advance(); continue; }
                None => break Token::Eof,
            }
        };
        TokenWithLine { token, line }
    }

    pub fn tokenize_all(&mut self) -> Vec<TokenWithLine> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token.token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
} 