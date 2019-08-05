use crate::chunk::{Chunk, Instruction};
use crate::value::Value;

const STACK_MAX: usize = 256;

struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

impl VM<'_> {
    fn new(chunk: &Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            stack: [Value::Garbage; STACK_MAX],
            stack_top: 0,
        }
    }

    fn run(&mut self) -> InterpretResult {
        let num_instructions = self.chunk.instruction_len();
        'interpret: while self.ip < num_instructions {
            if let Some(inst) = self.chunk.instruction_at_byte(self.ip) {
                dbg!(&inst);
                match inst {
                    Instruction::Return => {
                        dbg!(self.pop());
                        break 'interpret;
                    }
                    Instruction::Constant(index) => {
                        // TODO should pre-validate constant pool references and can skip the optional/unwrap here
                        let val = self.chunk.constant_at(index as usize).unwrap();
                        self.push(*val);
                    }
                    Instruction::Negate => match self.pop() {
                        Value::Double(d) => self.push(Value::Double(-d)),
                        Value::Garbage => return Err(InterpretError::UnknownValue(Value::Garbage)),
                    },
                }
                self.ip += inst.size();
            } else {
                return Err(InterpretError::UnknownInstruction(self.ip));
            }
        }
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }
}

enum InterpretError {
    UnknownInstruction(usize),
    UnknownValue(Value),
}

type InterpretResult = Result<(), InterpretError>;

#[cfg(test)]
mod tests {
    use crate::chunk::{Chunk, Instruction};
    use crate::value::Value;
    use crate::vm::VM;

    #[test]
    fn test() {
        let mut chunk = Chunk::new();
        chunk.add_constant(Value::Double(1.2));
        chunk.add_instruction(&Instruction::Constant(0), 1);
        chunk.add_instruction(&Instruction::Return, 2);
        dbg!(&chunk);
        let mut vm = VM::new(&chunk);
        vm.run();
    }
}
