use crate::chunk::{Chunk, Instruction};
use crate::compiler;
use crate::value::Value;
use std::fmt;

// TODO move to lib.rs? otherwise stuff in here is `vm::vm::Thing`

const STACK_MAX: usize = 256;

struct Stack {
    slots: [Value; STACK_MAX],
    top: usize,
}

impl Stack {
    fn new() -> Stack {
        Stack {
            slots: [Value::Nil; STACK_MAX],
            top: 0,
        }
    }
    fn push(&mut self, value: Value) {
        self.slots[self.top] = value;
        self.top += 1;
    }

    fn pop(&mut self) -> Value {
        self.top -= 1;
        let popped = self.slots[self.top];
        popped
    }
}

impl fmt::Debug for Stack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.slots[..self.top].fmt(f)
    }
}

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: Stack,
}

macro_rules! unary_op {
    ( $sel:ident, $op:tt) => {
        match $sel.stack.pop() {
            Value::Number(d) => $sel.stack.push(Value::Number( $op d)),
            v => return Err(InterpretError::TypeError($sel.error_msg(format!("Operand must be a number, got {}", v)))),
        }
    }
}

macro_rules! binary_op {
    ( $sel:ident, $op:tt, $result:ident) => {
        match $sel.stack.pop() {
            Value::Number(rhs) => match $sel.stack.pop() {
                Value::Number(lhs) => $sel.stack.push(Value::$result(lhs $op rhs)),
                v => return Err(InterpretError::TypeError($sel.error_msg(format!("Operand must be a number, got {}", v)))),
            },
            v => return Err(InterpretError::TypeError($sel.error_msg(format!("Operand must be a number, got {}", v)))),
        }
    }
}

impl VM<'_> {
    pub fn new(chunk: &Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn run(&mut self) -> InterpretResult {
        let constants = self.chunk.constants();
        let instructions = self.chunk.instructions();
        let num_instructions = instructions.len();
        'interpret: while self.ip < num_instructions {
            let inst = &instructions[self.ip];

            let fmt = format!("{}", inst);
            print!("{:04} {:<8}    ", self.ip, fmt);

            use Instruction::*;
            match inst {
                Return => {
                    let result = self.stack.pop();
                    return Ok(result);
                }
                Constant(index) => {
                    let val = &constants[*index as usize];
                    self.stack.push(*val);
                }
                Negate => unary_op!(self, -),
                Add => binary_op!(self, +, Number),
                Subtract => binary_op!(self, -, Number),
                Multiply => binary_op!(self, *, Number),
                Divide => binary_op!(self, /, Number),
                True => self.stack.push(Value::Boolean(true)),
                False => self.stack.push(Value::Boolean(false)),
                Nil => self.stack.push(Value::Nil),
                Not => {
                    let top = self.stack.pop();
                    self.stack.push(Value::Boolean(!VM::coerce_bool(&top)));
                },
                Equal => {
                    let rhs = self.stack.pop();
                    let lhs = self.stack.pop();
                    self.stack.push(Value::Boolean(VM::values_equal(&lhs, &rhs)));
                },
                Greater => binary_op!(self, >, Boolean),
                Less => binary_op!(self, <, Boolean),

            }

            println!("; stack: {:?}", self.stack);

            self.ip += 1;
        }
        Err(InterpretError::NoReturn)
    }

    fn error_msg(&self, str: String) -> String {
        format!("error at {}: {}", self.chunk.line_number(self.ip).unwrap_or(&(0 as usize)), str)
    }

    fn coerce_bool(v: &Value) -> bool {
        match v {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true
        }
    }

    fn values_equal(lhs: &Value, rhs: &Value) -> bool {
        use Value::*;
        match (lhs, rhs) {
            (Nil, Nil) => true,
            (Boolean(b1), Boolean(b2)) => b1 == b2,
            (Number(n1), Number(n2)) => n1 == n2,
            _ => false
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError(compiler::CompileError),
    UnknownInstruction(usize),
    UnknownValue(Value),
    TypeError(String),
    NoReturn
}

pub type InterpretResult = Result<Value, InterpretError>;

macro_rules! add_constant_instruction {
    ( $chunk:expr, $value:expr, $linenum:expr ) => {{
        let chunk: &mut Chunk = $chunk;
        let linenum: crate::chunk::LineNumber = $linenum;
        let idx: crate::chunk::ConstantPoolIndex = chunk.add_constant($value).unwrap_or_default();
        chunk.add_instruction(Instruction::Constant(idx), linenum);
    }};
}

#[cfg(test)]
mod tests {
    use crate::chunk::{Chunk, Instruction};
    use crate::value::Value;
    use crate::vm::VM;

    #[test]
    fn test() {
        let mut chunk = Chunk::new();
        add_constant_instruction!(&mut chunk, Value::Double(1.2), 123);
        add_constant_instruction!(&mut chunk, Value::Double(3.4), 123);
        chunk.add_instruction(Instruction::Add, 123);
        add_constant_instruction!(&mut chunk, Value::Double(5.6), 123);
        chunk.add_instruction(Instruction::Divide, 123);
        chunk.add_instruction(Instruction::Return, 123);
        dbg!(&chunk);
        let mut vm = VM::new(&chunk);
        vm.run();
    }
}
