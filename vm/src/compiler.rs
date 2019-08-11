use crate::chunk::{Chunk, LineNumber, Instruction, ChunkError};
use std::borrow::Cow;
use std::fmt;
use std::ops::Deref;
use crate::value::Value;
use crate::vm::InterpretError;

#[derive(Debug)]
pub enum CompileError {
    Error(String),
    ChunkError(ChunkError),
    Composite(Vec<CompileError>)
}

impl std::error::Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Into<CompileError> for ChunkError {
    fn into(self) -> CompileError {
        CompileError::ChunkError(self)
    }
}

impl From<CompileError> for InterpretError {
    fn from(e: CompileError) -> Self {
        InterpretError::CompileError(e)
    }
}

const DEBUG: bool = true;

pub fn compile(input: &str) -> Result<Chunk, CompileError> {
    let mut scanner = Scanner::new(input);
    let parser = Parser::new(&mut scanner);
    let result = parser.parse();
    if DEBUG {
        result.as_ref().map(|chunk| println!("{:?}", chunk));
    }
    result
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

    fn scan_token(&mut self) -> Token<'a> {
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
    ) -> Token<'a> {
        // my therapist says self.accept()ance is key
        let ty = if self.accept(c) {
            if_match
        } else {
            if_no_match
        };
        self.make_token(ty)
    }

    fn make_token(&mut self, ty: TokenType) -> Token<'a> {
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

    fn make_string(&mut self) -> Token<'a> {
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

    fn make_number(&mut self) -> Token<'a> {
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

    fn make_ident_or_kw(&mut self) -> Token<'a> {
        // we already checked is_alpha(curr) || curr == '_', so take it
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

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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
    scanner: &'a mut Scanner<'a>,
    curr: Token<'a>,
    prev: Token<'a>,
    errors: Vec<CompileError>
}

impl<'a> Parser<'a> {
    fn new(scanner: &'a mut Scanner<'a>) -> Parser<'a> {
        let initial_token = Token {
            ty: TokenType::Error,
            text: Cow::Borrowed("nothing scanned yet"),
            line: 1
        };
        Parser {
            chunk: Chunk::new(),
            scanner,
            curr: initial_token.clone(),
            prev: initial_token.clone(),
            errors: Vec::new()
        }
    }

    fn parse(mut self) -> Result<Chunk, CompileError> {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof, String::from("Expected end of expression."));

        self.end_compiler();

        if self.errors.is_empty() {
            Ok(self.chunk)
        } else {
            Err(CompileError::Composite(self.errors))
        }
    }

    // helpers

    fn advance(&mut self) {
        self.prev = self.curr.clone();
        loop {
            self.curr = self.scanner.scan_token();
            if self.curr.ty != TokenType::Error {
                break;
            } else {
                self.error_at_current(String::from(self.curr.text.deref()));
            }
        }
    }

    fn consume(&mut self, ty: TokenType, message: String) {
        if self.curr.ty == ty {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn current_chunk(&self) -> &Chunk {
        &self.chunk
    }

    fn error_at_current(&mut self, message: String) {
        // TODO avoid clone
        self.error_at(&self.curr.clone(), message);
    }

    fn error_at(&mut self, token: &Token, message: String) {
        self.errors.push(CompileError::Error(format!("[line {}] Error at {}: {}", token.line, if token.ty == TokenType::Eof { "end" } else { &*token.text }, message)));
    }

    fn emit(&mut self, instruction: Instruction) {
        self.chunk.add_instruction(instruction, self.curr.line);
    }

    fn emit_constant(&mut self, value: Value) {
        self.chunk.add_constant(value)
            .map(|offset| self.emit(Instruction::Constant(offset)))
            .map_err(|e| self.errors.push(e.into()));
    }

    fn end_compiler(&mut self) {
        self.emit(Instruction::Return)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_fn = Parser::get_rule(self.prev.ty).prefix;
        prefix_fn(self);

        while precedence <= Parser::get_rule(self.curr.ty).precedence {
            self.advance();
            let infix_fn = Parser::get_rule(self.prev.ty).infix;
            infix_fn(self);
        }
    }

    // grammar rules

    fn number(&mut self) {
        let parsed = self.prev.text.parse::<f64>().expect("number token was not parsable as double");
        self.emit_constant(Value::Double(parsed));
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, String::from("Expect ')' after expression"));
    }

    fn unary(&mut self) {
        let operator_type = self.prev.ty;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit(Instruction::Negate),
            _ => {
                // unreachable
                panic!("parser got confused about a unary expression");
            }
        }
    }

    fn binary(&mut self) {
        let operator_type = self.prev.ty;
        let parse_rule = Parser::get_rule(operator_type);
        self.parse_precedence(parse_rule.precedence.next().expect("already at highest precedence"));

        match operator_type {
            TokenType::Plus => self.emit(Instruction::Add),
            TokenType::Minus => self.emit(Instruction::Subtract),
            TokenType::Star => self.emit(Instruction::Multiply),
            TokenType::Slash => self.emit(Instruction::Divide),
            _ => panic!("unmatched binary expression")
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn get_rule(ty: TokenType) -> &'static ParseRule {
        match ty {
            TokenType::LeftParen    => &RULE_LEFT_PAREN,
            TokenType::RightParen   => &RULE_RIGHT_PAREN,
            TokenType::LeftBrace    => &RULE_LEFT_BRACE,
            TokenType::RightBrace   => &RULE_RIGHT_BRACE,
            TokenType::Comma        => &RULE_COMMA,
            TokenType::Dot          => &RULE_DOT,
            TokenType::Minus        => &RULE_MINUS,
            TokenType::Plus         => &RULE_PLUS,
            TokenType::Semicolon    => &RULE_SEMICOLON,
            TokenType::Slash        => &RULE_SLASH,
            TokenType::Star         => &RULE_STAR,
            TokenType::Bang         => &RULE_BANG,
            TokenType::BangEqual    => &RULE_BANG_EQUAL,
            TokenType::Equal        => &RULE_EQUAL,
            TokenType::EqualEqual   => &RULE_EQUAL_EQUAL,
            TokenType::Greater      => &RULE_GREATER,
            TokenType::GreaterEqual => &RULE_GREATER_EQUAL,
            TokenType::Less         => &RULE_LESS,
            TokenType::LessEqual    => &RULE_LESS_EQUAL,
            TokenType::Identifier   => &RULE_IDENTIFIER,
            TokenType::String       => &RULE_STRING,
            TokenType::Number       => &RULE_NUMBER,
            TokenType::And          => &RULE_AND,
            TokenType::Class        => &RULE_CLASS,
            TokenType::Else         => &RULE_ELSE,
            TokenType::False        => &RULE_FALSE,
            TokenType::For          => &RULE_FOR,
            TokenType::Fun          => &RULE_FUN,
            TokenType::If           => &RULE_IF,
            TokenType::Nil          => &RULE_NIL,
            TokenType::Or           => &RULE_OR,
            TokenType::Print        => &RULE_PRINT,
            TokenType::Return       => &RULE_RETURN,
            TokenType::Super        => &RULE_SUPER,
            TokenType::This         => &RULE_THIS,
            TokenType::True         => &RULE_TRUE,
            TokenType::Var          => &RULE_VAR,
            TokenType::While        => &RULE_WHILE,
            TokenType::Error        => &RULE_ERROR,
            TokenType::Eof          => &RULE_EOF,
        }
    }
}

#[derive(FromPrimitive, ToPrimitive, Ord, PartialOrd, Eq, PartialEq)]
enum Precedence {
    None = 0,
    Assignment = 1,  // =
    Or = 2,          // or
    And = 3,         // and
    Equality = 4,    // == !=
    Comparison = 5,  // < > <= >=
    Term = 6,        // + -
    Factor = 7,      // * /
    Unary = 8,       // ! -
    Call = 9,        // . () []
    Primary = 10
}

impl Precedence {
    fn next(&self) -> Option<Precedence> {
        let ordinal = num::ToPrimitive::to_i32(self).unwrap();
        num::FromPrimitive::from_i32(ordinal + 1)
    }
}

type ParseFn = fn(&mut Parser) -> ();

struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence
}

// TODO avoid clone
static FAIL: ParseFn = |p| { p.error_at(&p.prev.clone(), String::from("Expected expression")); };
// TODO how to make these just a reference to the method, eg, `Parser::grouping`, rather than a lambda over it?
static GROUPING: ParseFn = |p| p.grouping();
static UNARY: ParseFn = |p| p.unary();
static BINARY: ParseFn = |p| p.binary();
static NUMBER: ParseFn = |p| p.number();

static RULE_LEFT_PAREN: ParseRule    = ParseRule { prefix: GROUPING, infix: FAIL,   precedence: Precedence::None };
static RULE_RIGHT_PAREN: ParseRule   = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_LEFT_BRACE: ParseRule    = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_RIGHT_BRACE: ParseRule   = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_COMMA: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_DOT: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_MINUS: ParseRule         = ParseRule { prefix: UNARY,    infix: BINARY, precedence: Precedence::Term };
static RULE_PLUS: ParseRule          = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Term };
static RULE_SEMICOLON: ParseRule     = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_SLASH: ParseRule         = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Factor };
static RULE_STAR: ParseRule          = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Factor };
static RULE_BANG: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_BANG_EQUAL: ParseRule    = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_EQUAL: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_EQUAL_EQUAL: ParseRule   = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_GREATER: ParseRule       = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_GREATER_EQUAL: ParseRule = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_LESS: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_LESS_EQUAL: ParseRule    = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_IDENTIFIER: ParseRule    = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_STRING: ParseRule        = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_NUMBER: ParseRule        = ParseRule { prefix: NUMBER,   infix: FAIL,   precedence: Precedence::None };
static RULE_AND: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_CLASS: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_ELSE: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_FALSE: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_FOR: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_FUN: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_IF: ParseRule            = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_NIL: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_OR: ParseRule            = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_PRINT: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_RETURN: ParseRule        = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_SUPER: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_THIS: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_TRUE: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_VAR: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_WHILE: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_ERROR: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_EOF: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };

#[cfg(test)]
mod tests {
    use crate::compiler::{compile, Scanner, TokenType};

    #[test]
    fn compile_test() {
        let chunk = compile("(123 + 456) * -789");
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
