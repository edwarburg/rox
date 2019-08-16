use crate::chunk::{Chunk, LineNumber, Instruction, ChunkError, ConstantPoolIndex, MAX_CONSTANTS};
use std::borrow::{Cow, Borrow, BorrowMut};
use std::fmt;
use std::ops::Deref;
use crate::value::{Value, allocate_string};
use crate::vm::InterpretError;
use crate::context::LoxContext;
use crate::DEBUG;

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

const LOCALS_MAX: usize = (std::u8::MAX as usize) + 1;

#[derive(Debug, Clone)]
struct Local<'a> {
    token: Token<'a>,
    depth: i32
}

pub struct Compiler<'a> {
    context: &'a mut LoxContext,
    locals: Vec<Local<'a>>,
    scope_depth: i32,
}

impl<'a> Compiler<'a> {
    fn add_local(&mut self, token: Token<'a>) {
        self.locals.push(Local {
            token,
            depth: -1
        });
    }
}

pub fn compile<'a>(input: &'a str, context: &'a mut LoxContext) -> Result<Chunk, CompileError> {
    let mut scanner = Scanner::new(input);
    // TODO kinda weird we have this other thing called `Compiler` here, maybe there's a reason we'll need it later...
    let parser = Parser::new(&mut scanner, Compiler {
        context,
        locals: Vec::with_capacity(LOCALS_MAX),
        scope_depth: 0
    });
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

    fn make_string(&mut self) -> Token<'a> {
        // skip beginning '"'
        self.advance_start();
        let mut terminated = false;
        while let Some(c) = self.peek() {
            if c == '"' {
                terminated = true;
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

        let result = self.make_token(TokenType::String);
        // skip closing '"'
        self.take(1);
        self.advance_start();
        result
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
    compiler: Compiler<'a>,
    curr: Token<'a>,
    prev: Token<'a>,
    panic_mode: bool,
    errors: Vec<CompileError>
}

impl<'a> Parser<'a> {
    fn new(scanner: &'a mut Scanner<'a>, compiler: Compiler<'a>) -> Parser<'a> {
        let initial_token = Token {
            ty: TokenType::Error,
            text: Cow::Borrowed("nothing scanned yet"),
            line: 1
        };
        Parser {
            chunk: Chunk::new(),
            scanner,
            compiler,
            curr: initial_token.clone(),
            prev: initial_token.clone(),
            panic_mode: false,
            errors: Vec::new()
        }
    }

    fn parse(mut self) -> Result<Chunk, CompileError> {
        self.advance();

        while !self.maybe_consume(TokenType::Eof) {
            self.declaration();
        }

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

    fn maybe_consume(&mut self, ty: TokenType) -> bool {
        if self.curr.ty == ty {
            self.advance();
            return true;
        }

        false
    }

    fn error_at_current(&mut self, message: String) {
        // TODO avoid clone
        self.error_at(&self.curr.clone(), message);
    }

    fn error_at(&mut self, token: &Token, message: String) {
        self.push_error(CompileError::Error(format!("[line {}] Error at {}: {}", token.line, if token.ty == TokenType::Eof { "end" } else { &*token.text }, message)));
    }

    fn push_error(&mut self, error: CompileError) {
        if !self.panic_mode {
            self.panic_mode = true;
            self.errors.push(error);
        }
    }

    fn emit(&mut self, instruction: Instruction) {
        self.chunk.add_instruction(instruction, self.curr.line);
    }

    fn emit2(&mut self, instruction1: Instruction, instruction2: Instruction) {
        self.emit(instruction1);
        self.emit(instruction2);
    }

    fn emit_constant(&mut self, value: Value) -> ConstantPoolIndex {
        self.chunk.add_constant(value)
            .map(|offset| { self.emit(Instruction::Constant(offset)); offset })
            .map_err(|e| self.push_error(e.into()))
            .unwrap_or(0 as ConstantPoolIndex)
    }

    fn end_compiler(&mut self) {
        self.emit(Instruction::Return)
    }

    // grammar rules

    fn declaration(&mut self) {
        println!("declaration");
        if self.maybe_consume(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        use crate::compiler::TokenType::*;
        while self.curr.ty != Eof {
            if self.prev.ty == Semicolon {
                return;
            }


            match self.curr.ty {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    fn var_declaration(&mut self) {
        let variable_name_index = self.parse_variable("Expect variable name.".to_owned());
        if self.maybe_consume(TokenType::Equal) {
            self.expression();
        } else {
            self.emit(Instruction::Nil);
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.".to_owned());
        self.define_variable(variable_name_index);
    }

    fn parse_variable(&mut self, error_msg: String) -> ConstantPoolIndex {
        self.consume(TokenType::Identifier, error_msg);

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        let cow = self.prev.text.clone();
        let var_name = cow.deref();
        self.identifier_constant(var_name)
    }

    fn identifier_constant(&mut self, str: &str) -> ConstantPoolIndex {
        let obj_ref = allocate_string(str, self.compiler.context);
        self.chunk.add_constant(Value::Object(obj_ref))
            .map_err(|e| self.push_error(e.into()))
            .unwrap_or(MAX_CONSTANTS as ConstantPoolIndex)
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        for i in (0..self.compiler.locals.len()).rev() {
            let local = &self.compiler.locals[i];
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }

            if local.token.text.deref().eq(self.prev.text.deref()) {
                self.error_at_current("Variable with this name already defined in this scope.".to_owned());
            }
        }
        self.compiler.add_local(self.prev.clone())
    }

    fn define_variable(&mut self, index: ConstantPoolIndex) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit(Instruction::DefineGlobal(index));
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        let local = self.compiler.locals.last_mut().unwrap();

        local.depth = self.compiler.scope_depth;
    }

    fn statement(&mut self) {
        if self.maybe_consume(TokenType::Print) {
            self.print_statement();
        } else if self.maybe_consume(TokenType::LeftBrace) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value.".to_owned());
        self.emit(Instruction::Print);
    }

    fn beginScope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn endScope(&mut self) {
        self.compiler.scope_depth -= 1;

        while let Some(true) = self.compiler.locals.last().map(|l| l.depth > self.compiler.scope_depth) {
            self.emit(Instruction::Pop);
            self.compiler.locals.pop();
        }
    }

    fn block(&mut self) {
        while self.curr.ty != TokenType::RightBrace && self.curr.ty != TokenType::Eof {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.".to_owned());
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after expression.".to_owned());
        self.emit(Instruction::Pop);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_fn = Parser::get_rule(self.prev.ty).prefix;
        let can_assign = precedence <= Precedence::Assignment;
        prefix_fn(self, can_assign);

        while precedence <= Parser::get_rule(self.curr.ty).precedence {
            self.advance();
            let infix_fn = Parser::get_rule(self.prev.ty).infix;
            infix_fn(self, can_assign);
        }

        if can_assign && self.maybe_consume(TokenType::Equal) {
            self.error_at_current("Invalid assignment target.".to_owned());
            self.expression();
        }
    }

    fn number(&mut self, can_assign: bool) {
        let parsed = self.prev.text.parse::<f64>().expect("number token was not parsable as double");
        self.emit_constant(Value::Number(parsed));
    }

    fn string(&mut self, can_assign: bool) {
        let string = String::from(self.prev.text.deref());
        let loxstr = crate::value::allocate_string(string.borrow(), self.compiler.context);
        self.emit_constant(Value::Object(loxstr));
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.prev.clone(), can_assign);
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let mut get_op = Instruction::Nil;
        let mut set_op = Instruction::Nil;
        if let Some(arg) = self.resolve_local(name) {
            get_op = Instruction::GetLocal(arg);
            set_op = Instruction::SetLocal(arg);
        } else {
            let arg = self.identifier_constant(name.text.deref());
            get_op = Instruction::GetGlobal(arg);
            set_op = Instruction::SetGlobal(arg);
        }

        if can_assign && self.maybe_consume(TokenType::Equal) {
            // this is the lhs of an assignment, eg, `a = 2;`
            self.expression();
            self.emit(set_op);
        } else {
            // this is a normal reference to a variable
            self.emit(get_op);
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<ConstantPoolIndex> {
        for i in (0..self.compiler.locals.len()).rev() {
            let local = &self.compiler.locals[i];

            if local.token.text.deref().eq(self.prev.text.deref()) {
                if local.depth == -1 {
                    self.error_at_current("Cannot read local variable in its own initializer.".to_owned());
                }
                return Some(i as ConstantPoolIndex);
            }
        }

        None
    }

    fn grouping(&mut self, can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, String::from("Expect ')' after expression"));
    }

    fn literal(&mut self, can_assign: bool) {
        match self.prev.ty {
            TokenType::True => self.emit(Instruction::True),
            TokenType::False => self.emit(Instruction::False),
            TokenType::Nil => self.emit(Instruction::Nil),
            _ => {
                // unreachable
                panic!("parser got confused about literal");
            }
        }
    }

    fn unary(&mut self, can_assign: bool) {
        let operator_type = self.prev.ty;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit(Instruction::Negate),
            TokenType::Bang => self.emit(Instruction::Not),
            _ => {
                // unreachable
                panic!("parser got confused about a unary expression");
            }
        }
    }

    fn binary(&mut self, can_assign: bool) {
        let operator_type = self.prev.ty;
        let parse_rule = Parser::get_rule(operator_type);
        self.parse_precedence(parse_rule.precedence.next().expect("already at highest precedence"));

        match operator_type {
            TokenType::Plus => self.emit(Instruction::Add),
            TokenType::Minus => self.emit(Instruction::Subtract),
            TokenType::Star => self.emit(Instruction::Multiply),
            TokenType::Slash => self.emit(Instruction::Divide),
            TokenType::BangEqual => self.emit2(Instruction::Equal, Instruction::Not),
            TokenType::EqualEqual => self.emit(Instruction::Equal),
            TokenType::Greater => self.emit(Instruction::Greater),
            TokenType::GreaterEqual => self.emit2(Instruction::Less, Instruction::Not),
            TokenType::Less => self.emit(Instruction::Less),
            TokenType::LessEqual => self.emit2(Instruction::Greater, Instruction::Not),
            _ => panic!("unmatched binary expression")
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn get_rule(ty: TokenType) -> &'static ParseRule {
        use TokenType::*;
        match ty {
            LeftParen    => &RULE_LEFT_PAREN,
            RightParen   => &RULE_RIGHT_PAREN,
            LeftBrace    => &RULE_LEFT_BRACE,
            RightBrace   => &RULE_RIGHT_BRACE,
            Comma        => &RULE_COMMA,
            Dot          => &RULE_DOT,
            Minus        => &RULE_MINUS,
            Plus         => &RULE_PLUS,
            Semicolon    => &RULE_SEMICOLON,
            Slash        => &RULE_SLASH,
            Star         => &RULE_STAR,
            Bang         => &RULE_BANG,
            BangEqual    => &RULE_BANG_EQUAL,
            Equal        => &RULE_EQUAL,
            EqualEqual   => &RULE_EQUAL_EQUAL,
            Greater      => &RULE_GREATER,
            GreaterEqual => &RULE_GREATER_EQUAL,
            Less         => &RULE_LESS,
            LessEqual    => &RULE_LESS_EQUAL,
            Identifier   => &RULE_IDENTIFIER,
            String       => &RULE_STRING,
            Number       => &RULE_NUMBER,
            And          => &RULE_AND,
            Class        => &RULE_CLASS,
            Else         => &RULE_ELSE,
            False        => &RULE_FALSE,
            For          => &RULE_FOR,
            Fun          => &RULE_FUN,
            If           => &RULE_IF,
            Nil          => &RULE_NIL,
            Or           => &RULE_OR,
            Print        => &RULE_PRINT,
            Return       => &RULE_RETURN,
            Super        => &RULE_SUPER,
            This         => &RULE_THIS,
            True         => &RULE_TRUE,
            Var          => &RULE_VAR,
            While        => &RULE_WHILE,
            Error        => &RULE_ERROR,
            Eof          => &RULE_EOF,
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

type ParseFn = fn(&mut Parser, bool) -> ();

struct ParseRule {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence
}

// TODO avoid clone
static FAIL: ParseFn = |p, _| { p.error_at(&p.prev.clone(), String::from("Expected expression")); };
// TODO how to make these just a reference to the method, eg, `Parser::grouping`, rather than a lambda over it?
static GROUPING: ParseFn = |p, a| p.grouping(a);
static LITERAL: ParseFn = |p, a| p.literal(a);
static UNARY: ParseFn = |p, a| p.unary(a);
static BINARY: ParseFn = |p, a| p.binary(a);
static NUMBER: ParseFn = |p, a| p.number(a);
static STRING: ParseFn = |p, a| p.string(a);
static VARIABLE: ParseFn = |p, a| p.variable(a);

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
static RULE_BANG: ParseRule          = ParseRule { prefix: UNARY,    infix: FAIL,   precedence: Precedence::None };
static RULE_BANG_EQUAL: ParseRule    = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Equality };
static RULE_EQUAL: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_EQUAL_EQUAL: ParseRule   = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Equality};
static RULE_GREATER: ParseRule       = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Comparison };
static RULE_GREATER_EQUAL: ParseRule = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Comparison };
static RULE_LESS: ParseRule          = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Comparison };
static RULE_LESS_EQUAL: ParseRule    = ParseRule { prefix: FAIL,     infix: BINARY, precedence: Precedence::Comparison };
static RULE_IDENTIFIER: ParseRule    = ParseRule { prefix: VARIABLE, infix: FAIL,   precedence: Precedence::None };
static RULE_STRING: ParseRule        = ParseRule { prefix: STRING,   infix: FAIL,   precedence: Precedence::None };
static RULE_NUMBER: ParseRule        = ParseRule { prefix: NUMBER,   infix: FAIL,   precedence: Precedence::None };
static RULE_AND: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_CLASS: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_ELSE: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_FALSE: ParseRule         = ParseRule { prefix: LITERAL,  infix: FAIL,   precedence: Precedence::None };
static RULE_FOR: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_FUN: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_IF: ParseRule            = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_NIL: ParseRule           = ParseRule { prefix: LITERAL,  infix: FAIL,   precedence: Precedence::None };
static RULE_OR: ParseRule            = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_PRINT: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_RETURN: ParseRule        = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_SUPER: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_THIS: ParseRule          = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_TRUE: ParseRule          = ParseRule { prefix: LITERAL,  infix: FAIL,   precedence: Precedence::None };
static RULE_VAR: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_WHILE: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_ERROR: ParseRule         = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };
static RULE_EOF: ParseRule           = ParseRule { prefix: FAIL,     infix: FAIL,   precedence: Precedence::None };

#[cfg(test)]
mod tests {
    use crate::compiler::{compile, Scanner, TokenType, Compiler};
    use crate::context::LoxContext;

    #[test]
    fn compile_test() {
        let mut context = LoxContext::new();
        let chunk = compile("(123 + 456) * -789", &mut context);
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
