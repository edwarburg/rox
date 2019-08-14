use crate::chunk::{Chunk, Instruction};
use crate::{compiler, DEBUG};
use crate::value::{Value, allocate_string};
use crate::context::LoxContext;
use std::borrow::Borrow;

// TODO move to lib.rs? otherwise stuff in here is `vm::vm::Thing`

const STACK_MAX: usize = 256;

#[derive(Debug)]
struct Stack {
    slots: Vec<Value>
}

impl Stack {
    fn new() -> Stack {
        Stack {
            slots: Vec::new()
        }
    }
    fn push(&mut self, value: Value) {
        self.slots.push(value);
    }

    fn pop(&mut self) -> Result<Value, InterpretError> {
        self.slots.pop().ok_or(InterpretError::PoppedEmptyStack)
    }
}

pub struct VM<'a> {
    chunk: &'a Chunk,
    context: &'a mut LoxContext,
    ip: usize,
    stack: Stack,
}

macro_rules! unary_op {
    ( $sel:ident, $op:tt) => {
        match $sel.stack.pop()? {
            Value::Number(d) => $sel.stack.push(Value::Number( $op d)),
            v => return Err($sel.operand_type_error(stringify!($result), v)),
        }
    }
}

macro_rules! binary_op {
    ( $sel:ident, $op:tt, $result:ident) => {
        match $sel.stack.pop()? {
            Value::Number(rhs) => match $sel.stack.pop()? {
                Value::Number(lhs) => $sel.stack.push(Value::$result(lhs $op rhs)),
                v => return Err($sel.operand_type_error(stringify!($result), v)),
            },
            v => return Err($sel.operand_type_error(stringify!($result), v)),
        }
    }
}

#[derive(Debug)]
pub enum InterpretError {
    CompileError(compiler::CompileError),
    UnknownInstruction(usize),
    UnknownValue(Value),
    TypeError(String),
    NoReturn,
    PoppedEmptyStack
}


impl VM<'_> {
    pub fn new<'a>(chunk: &'a Chunk, context: &'a mut LoxContext) -> VM<'a> {
        VM {
            chunk,
            context,
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn run(&mut self) -> InterpretResult {
        let constants = self.chunk.constants();
        let instructions = self.chunk.instructions();
        let num_instructions = instructions.len();
        defer!(if DEBUG {
            print!("\n\n")
        });

        'interpret: while self.ip < num_instructions {
            let inst = &instructions[self.ip];

            let fmt = format!("{}", inst);
            if DEBUG {
                print!("{:04} {:<8}    ", self.ip, fmt);
            }

            use Instruction::*;
            match inst {
                Return => {
                    return Ok(());
                }
                Constant(index) => {
                    let val = &constants[*index as usize];
                    self.stack.push(val.clone());
                }
                Negate => unary_op!(self, -),
                Add => {
                    let rhs = self.stack.pop()?;
                    let lhs = self.stack.pop()?;
                    let result = match (&lhs, &rhs) {
                        (Value::Number(n1), Value::Number(n2)) => Value::Number(n1 + n2),
                        (lhs @ Value::Object(_), rhs @ Value::Object(_)) if lhs.is_string() && rhs.is_string() => {
                            Value::Object(allocate_string([lhs.as_string(), rhs.as_string()].concat().borrow(), self.context))
                        }
                        _ => return Err(InterpretError::TypeError(format!("Cannot add {} and {} because they are the wrong type(s)", lhs, rhs)))
                    };
                    self.stack.push(result);
                }
                Subtract => binary_op!(self, -, Number),
                Multiply => binary_op!(self, *, Number),
                Divide => binary_op!(self, /, Number),
                True => self.stack.push(Value::Boolean(true)),
                False => self.stack.push(Value::Boolean(false)),
                Nil => self.stack.push(Value::Nil),
                Not => {
                    let top = self.stack.pop()?;
                    self.stack.push(Value::Boolean(!VM::coerce_bool(&top)));
                },
                Equal => {
                    let rhs = self.stack.pop()?;
                    let lhs = self.stack.pop()?;
                    self.stack.push(Value::Boolean(VM::values_equal(&lhs, &rhs)));
                },
                Greater => binary_op!(self, >, Boolean),
                Less => binary_op!(self, <, Boolean),
                Print => {
                    let to_print = self.stack.pop()?;
                    if DEBUG {
                        print!("\nstout ==> ");
                    }
                    println!("{}", &to_print);

                },
                Pop => {
                    self.stack.pop()?;
                }
            }

            if DEBUG {
                println!("; stack: {:?}", self.stack);
            }

            self.ip += 1;
        }
        Err(InterpretError::NoReturn)
    }

    fn error_msg(&self, str: String) -> String {
        format!("error at {}: {}", self.chunk.line_number(self.ip).unwrap_or(&(0 as usize)), str)
    }

    fn operand_type_error(&self, type_as_str: &str, v: Value) -> InterpretError {
        InterpretError::TypeError(self.error_msg(format!("Operand must be a {}, but got {}", type_as_str, v)))
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
            (Object(o1), Object(o2)) => o1 == o2,
            _ => false
        }
    }
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
    use crate::context::LoxContext;

    #[test]
    fn test() {
        let mut chunk = Chunk::new();
        add_constant_instruction!(&mut chunk, Value::Number(1.2), 123);
        add_constant_instruction!(&mut chunk, Value::Number(3.4), 123);
        chunk.add_instruction(Instruction::Add, 123);
        add_constant_instruction!(&mut chunk, Value::Number(5.6), 123);
        chunk.add_instruction(Instruction::Divide, 123);
        chunk.add_instruction(Instruction::Return, 123);
        dbg!(&chunk);
        let mut context = LoxContext::new();
        let mut vm = VM::new(&chunk, &mut context);
        vm.run();
    }
}
