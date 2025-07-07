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

pub struct Lexer<'a> {
    chars: Chars<'a>,
    current_char: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current_char = chars.next();
        Lexer {
            chars,
            current_char,
        }
    }

    fn advance(&mut self) {
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

    pub fn next_token(&mut self) -> Token {
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
                self.next_token()
            }
            Some(':') => { self.advance(); Token::Colon }
            Some(',') => { self.advance(); Token::Comma }
            Some('(') => { self.advance(); Token::LParen }
            Some(')') => { self.advance(); Token::RParen }
            Some('{') => { self.advance(); Token::LBrace }
            Some('}') => { self.advance(); Token::RBrace }
            Some(';') => { self.advance(); Token::Semicolon }
            Some('+') => { self.advance(); Token::Plus }
            Some('-') => { self.advance(); Token::Minus }
            Some('*') => { self.advance(); Token::Star }
            Some('/') => { self.advance(); Token::Slash }
            Some('=') => {
                self.advance();
                if self.current_char == Some('=') { self.advance(); Token::EqEq } else { Token::Eq }
            }
            Some('!') => {
                self.advance();
                if self.current_char == Some('=') { self.advance(); Token::NotEq } else { Token::Not }
            }
            Some('<') => {
                self.advance();
                if self.current_char == Some('=') { self.advance(); Token::Le } else { Token::Lt }
            }
            Some('>') => {
                self.advance();
                if self.current_char == Some('=') { self.advance(); Token::Ge } else { Token::Gt }
            }
            Some('"') => { let s = self.read_string(); Token::StrLiteral(s) }
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
                    Token::FloatLiteral(num.parse().unwrap_or(0.0))
                } else {
                    Token::IntLiteral(num.parse().unwrap_or(0))
                }
            }
            Some(c) if c.is_alphabetic() || c == '_' => {
                let ident = self.read_identifier();
                match ident.as_str() {
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
                }
            }
            Some(_) => { self.advance(); self.next_token() }
            None => Token::Eof,
        }
    }

    pub fn tokenize_all(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
} 