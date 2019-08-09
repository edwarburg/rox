use crate::chunk::{Chunk, LineNumber};
use std::borrow::Cow;
use std::fmt;

#[derive(Debug)]
pub enum CompileError {
    Error(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for CompileError {}

pub fn compile(input: &str) -> Result<Chunk, CompileError> {
    let scanner = Scanner::new(input);
    let mut parser = Parser::new(&scanner);
    parser.parse()
}

#[derive(Debug)]
struct Scanner<'a> {
    start: &'a str,
    curr_len: usize,
    line: LineNumber,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Scanner<'a> {
        Scanner {
            start: input,
            curr_len: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        if let Some(c) = self.advance() {
            match c {
                '(' => return self.make_token(TokenType::LeftParen),
                ')' => return self.make_token(TokenType::RightParen),
                '{' => return self.make_token(TokenType::LeftBrace),
                '}' => return self.make_token(TokenType::RightBrace),
                ';' => return self.make_token(TokenType::Semicolon),
                ',' => return self.make_token(TokenType::Comma),
                '.' => return self.make_token(TokenType::Dot),
                '-' => return self.make_token(TokenType::Minus),
                '+' => return self.make_token(TokenType::Plus),
                '/' => return self.make_token(TokenType::Slash),
                '*' => return self.make_token(TokenType::Star),

                '!' => return self.if_accept_then_make('=', TokenType::BangEqual, TokenType::Bang),
                '=' => {
                    return self.if_accept_then_make('=', TokenType::EqualEqual, TokenType::Equal)
                }
                '<' => return self.if_accept_then_make('=', TokenType::LessEqual, TokenType::Less),
                '>' => {
                    return self.if_accept_then_make(
                        '=',
                        TokenType::GreaterEqual,
                        TokenType::Greater,
                    )
                }

                '"' => return self.make_string(),

                '0'..='9' => return self.make_number(),

                'a'..='z' | 'A'..='Z' | '_' => return self.make_ident_or_kw(),

                _ => return self.error_token(format!("unexpected character: '{}'", c)),
            }
        } else {
            return self.error_token("Tried to advance past EOF");
        }

        return self.error_token("unknown token");
    }

    fn is_at_end(&self) -> bool {
        self.start.is_empty()
    }

    fn take_and_make(&mut self, chars: usize, ty: TokenType) -> Token {
        self.take(chars);
        self.make_token(ty)
    }

    fn take(&mut self, chars: usize) {
        self.curr_len += chars;
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.take(1);
        }
        c
    }

    fn accept(&mut self, c: char) -> bool {
        if let Some(peeked) = self.peek() {
            if peeked == c {
                self.take(1);
                return true;
            }
        }

        return false;
    }

    fn peek(&self) -> Option<char> {
        self.peek_at(0)
    }

    fn peek_at(&self, i: usize) -> Option<char> {
        // TODO keep self.chars() around in Scanner for efficiency, maybe
        self.start.chars().nth(self.curr_len + i)
    }

    fn matches_at<F>(&self, i: usize, f: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        self.peek_at(i).map_or(false, f)
    }

    fn unsafe_peek(&self) -> char {
        self.peek().unwrap()
    }

    fn if_accept_then_make(
        &mut self,
        c: char,
        if_match: TokenType,
        if_no_match: TokenType,
    ) -> Token {
        // my therapist says self.accept()ance is key
        let ty = if self.accept(c) {
            if_match
        } else {
            if_no_match
        };
        self.make_token(ty)
    }

    fn make_token(&mut self, ty: TokenType) -> Token {
        let token = Token::new(ty, &self.start[..self.curr_len], self.line);
        self.advance_start();
        token
    }

    fn error_token<S>(&self, message: S) -> Token<'a>
    where
        S: Into<Cow<'a, str>>,
    {
        Token::new(TokenType::Error, message, self.line)
    }

    fn advance_start(&mut self) {
        // TODO more efficient way to work our way through the string? this is good enough for now I guess...
        if self.curr_len != 0 {
            self.start = &self.start[self.curr_len..];
            self.curr_len = 0;
        }
    }

    fn skip_whitespace(&mut self) {
        'outer: loop {
            if let Some(peeked) = self.peek() {
                match peeked {
                    ' ' | '\r' | '\t' => self.take(1),
                    '\n' => {
                        self.take(1);
                        self.line += 1;
                    }
                    '/' => {
                        if self.matches_at(1, |c| c == '/') {
                            self.take(2);
                            while let Some(c) = self.peek() {
                                self.take(1);
                                if c == '\n' {
                                    break;
                                }
                            }
                        } else {
                            break 'outer;
                        }
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        self.advance_start();
    }

    fn skip(&mut self, n: usize) {
        self.take(n);
        self.advance_start();
    }

    fn make_string(&mut self) -> Token {
        // we already checked curr == '"', so take it
        self.take(1);
        let mut terminated = false;
        while let Some(c) = self.peek() {
            if c == '"' {
                terminated = true;
                self.take(1);
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            self.take(1);
        }

        if !terminated {
            return self.error_token("Unterminated string literal");
        }

        self.make_token(TokenType::String)
    }

    fn make_number(&mut self) -> Token {
        // we already checked is_digit(curr), so take it
        self.take(1);
        while self.matches_at(0, |c| is_digit(c)) {
            self.take(1);
        }

        if self.matches_at(0, |c| c == '.') && self.matches_at(1, |c| is_digit(c)) {
            self.take(1);
            while self.matches_at(0, |c| is_digit(c)) {
                self.take(1);
            }
        }

        self.make_token(TokenType::Number)
    }

    fn make_ident_or_kw(&mut self) -> Token {
        // we already checked is_alpha(curr) || curr == '_', so take it
        self.take(1);
        while self.matches_at(0, |c| is_valid_ident_char(c)) {
            self.take(1);
        }
        // TODO could have done the fancy trie thing from the book... but this is much easier
        let ty = match &self.start[..self.curr_len] {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        self.make_token(ty)
    }
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_valid_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[derive(Debug, Eq, PartialEq)]
struct Token<'a> {
    ty: TokenType,
    text: Cow<'a, str>,
    line: LineNumber,
}

impl<'a> Token<'a> {
    pub fn new<S>(ty: TokenType, text: S, line: LineNumber) -> Token<'a>
    where
        S: Into<Cow<'a, str>>,
    {
        Token {
            ty,
            text: text.into(),
            line,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum TokenType {
    // Single-character tokens.
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

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
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

    // special
    Error,
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

struct Parser<'a> {
    chunk: Chunk,
    scanner: &'a Scanner<'a>,
    curr: Option<Token<'a>>,
    prev: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    fn new(scanner: &'a Scanner) -> Parser<'a> {
        Parser {
            chunk: Chunk::new(),
            scanner,
            curr: None,
            prev: None,
        }
    }

    fn parse(&mut self) -> Result<Chunk, CompileError> {
        unimplemented!()
    }

    fn consume(&mut self) -> Token {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{compile, Scanner, TokenType};

    #[test]
    fn compile_test() {
        let chunk = compile("");
        print!("{:?}", chunk);
    }

    #[test]
    fn scanner_test() {
        let input = "*()+   /!!=\t<\r<=>>=\n\n===// this is a comment ;*\n!\"this is a string !@#$abc\"123 123.456 123. abc var return true yes";
        let mut scanner = Scanner {
            start: input,
            curr_len: 0,
            line: 1,
        };

        let mut line = 0;
        loop {
            let token = scanner.scan_token();
            if token.line != line {
                print!("{:4} ", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }

            // TODO Why doesn't padding work... oh well.
            println!("{:>12} '{}'", token.ty, token.text);

            if token.ty == TokenType::Eof {
                break;
            }

            // TODO continue past error by consuming error token text when making error token
            if token.ty == TokenType::Error {
                break;
            }
        }
    }
}
