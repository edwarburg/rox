use crate::chunk::{Chunk, LineNumber};
use std::fmt;

pub fn compile(input: &str) -> Chunk {
    // TODO actually compile
    let chunk = Chunk::new();
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

        if token.ty == TokenType::Error {
            break;
        }
    }
    chunk
}

#[derive(Debug)]
struct Scanner<'a> {
    start: &'a str,
    curr_len: usize,
    line: LineNumber,
}

impl Scanner<'_> {
    fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = match self.advance() {
            Some(c) => c,
            None => return self.error_token("Tried to advance past EOF"),
        };

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
            '=' => return self.if_accept_then_make('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => return self.if_accept_then_make('=', TokenType::LessEqual, TokenType::Less),
            '>' => {
                return self.if_accept_then_make('=', TokenType::GreaterEqual, TokenType::Greater)
            }

            _ => return self.error_token("unexpected character"),
        }

        // TODO how to format error message here? calling format!() creates a temporary string that then gets returned in the Token and doesn't live long enough.
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
        //        dbg!(chars);
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
            //            dbg!(peeked);
            if peeked == c {
                self.take(1);
                return true;
            }
        }

        return false;
    }

    fn peek(&self) -> Option<char> {
        self.start.chars().nth(self.curr_len)
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
        //        dbg!(&self, &ty);
        self.make_token(ty)
    }

    fn make_token(&mut self, ty: TokenType) -> Token {
        let token = Token {
            ty,
            text: &self.start[..self.curr_len],
            line: self.line,
        };
        self.advance_start();
        token
    }

    fn error_token<'a>(&self, message: &'a str) -> Token<'a> {
        Token {
            ty: TokenType::Error,
            text: message,
            line: self.line,
        }
    }

    fn advance_start(&mut self) {
        // TODO more efficient way to work our way through the string? this is good enough for now I guess...
        if self.curr_len != 0 {
            self.start = &self.start[self.curr_len..];
            self.curr_len = 0;
        }
    }

    fn skip_whitespace(&mut self) {
        let mut skipped: usize = 0;
        loop {
            if let Some(peeked) = self.peek() {
                match peeked {
                    ' ' | '\r' | '\t' => self.take(1),
                    '\n' => {
                        self.take(1);
                        self.line += 1;
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
}

#[derive(Debug, Eq, PartialEq)]
struct Token<'a> {
    ty: TokenType,
    text: &'a str,
    line: LineNumber,
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

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        let chunk = crate::compiler::compile("*()+   /!!=\t<\r<=>>=\n\n===");
        println!("{:?}", &chunk);
    }
}
