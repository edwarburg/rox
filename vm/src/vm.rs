use crate::chunk::{Chunk, Instruction};
use crate::compiler;
use crate::compiler::CompileError;
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
            slots: [Value::Garbage; STACK_MAX],
            top: 0,
        }
    }
    fn push(&mut self, value: Value) {
        self.slots[self.top] = value;
        self.top += 1;
        eprintln!("pushed: {:?}, stack: {:?}", value, self);
    }

    fn pop(&mut self) -> Value {
        self.top -= 1;
        let popped = self.slots[self.top];
        eprintln!("popped: {:?}, stack: {:?}", popped, self);
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
            Value::Double(d) => $sel.stack.push(Value::Double( $op d)),
            v => return Err(InterpretError::UnknownValue(v)),
        }
    }
}

macro_rules! binary_op {
    ( $sel:ident, $op:tt) => {
        match $sel.stack.pop() {
            Value::Double(rhs) => match $sel.stack.pop() {
                Value::Double(lhs) => $sel.stack.push(Value::Double(lhs $op rhs)),
                v => return Err(InterpretError::UnknownValue(v))
            },
            v => return Err(InterpretError::UnknownValue(v)),
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
            dbg!(inst);
            match inst {
                Instruction::Return => {
                    self.stack.pop();
                    break 'interpret;
                }
                Instruction::Constant(index) => {
                    let val = &constants[*index as usize];
                    self.stack.push(*val);
                }
                Instruction::Negate => unary_op!(self, -),
                Instruction::Add => binary_op!(self, +),
                Instruction::Subtract => binary_op!(self, -),
                Instruction::Multiply => binary_op!(self, *),
                Instruction::Divide => binary_op!(self, /),
            }
            self.ip += 1;
        }
        Ok(())
    }
}

pub enum InterpretError {
    CompileError(compiler::CompileError),
    UnknownInstruction(usize),
    UnknownValue(Value),
}

pub type InterpretResult = Result<(), InterpretError>;

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
