extern crate enum_repr;

use crate::value::Value;
use enum_repr::EnumRepr;
use std::fmt;

type ConstantPoolIndex = u8;
type LineNumber = usize;

pub struct Chunk {
    bytes: Vec<u8>,
    constant_pool: Vec<Value>,
    lines: Vec<LineNumber>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            bytes: Vec::new(),
            constant_pool: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, constant: Value) {
        self.constant_pool.push(constant);
    }

    pub fn constant_at(&self, index: usize) -> Option<&Value> {
        self.constant_pool.get(index)
    }

    pub fn add_instruction(&mut self, instruction: &Instruction, line: LineNumber) {
        self.lines.push(line);
        instruction.encode(&mut self.bytes);
    }

    pub fn instruction_at_byte(&self, index: usize) -> Option<Instruction> {
        self.bytes
            .get(index..)
            .and_then(|slice| Instruction::decode(slice))
    }

    pub fn instruction_len(&self) -> usize {
        self.bytes.len()
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut i: usize = 0;
        let inst_size = self.bytes.len();
        if inst_size == 0 {
            write!(f, "<empty>");
            return Ok(());
        }
        let mut inst_num = 0;
        while i < inst_size {
            write!(f, "{:04} {:4}     ", i, self.lines[inst_num]);
            if let Some(inst) = Instruction::decode(&self.bytes[i..]) {
                write!(f, "{}", inst)?;
                i += inst.size();

                match inst {
                    Instruction::Constant(index) => {
                        write!(f, "    ; value: {}", self.constant_pool[index as usize])?
                    }
                    _ => {}
                }
            } else {
                write!(f, "UNKNOWN INSTRUCTION {}", self.bytes[i])?;
            }
            write!(f, "\n");
            inst_num += 1;
        }
        Ok(())
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Chunk (\n{})", self)
    }
}

#[derive(Debug, Eq, PartialOrd, PartialEq)]
pub enum Instruction {
    Return,
    Constant(ConstantPoolIndex),
    Negate,
}

// TODO lots of low hanging fruit in this file for writing macros mapping back and forth between OpCode and Instruction

impl Instruction {
    fn decode(bytes: &[u8]) -> Option<Instruction> {
        bytes
            .get(0)
            .and_then(|byte| OpCode::from_repr(*byte))
            .and_then(|op| match op {
                OpCode::Return => Some(Instruction::Return),
                OpCode::Constant => bytes.get(1).map(|byte| Instruction::Constant(*byte)),
                OpCode::Negate => Some(Instruction::Negate),
            })
    }

    // TODO byte sink as output rather than Vec?
    // TODO or figure out serde?
    fn encode(&self, output: &mut Vec<u8>) {
        match self {
            Instruction::Return => output.push(OpCode::Return.repr()),
            Instruction::Constant(index) => {
                output.push(OpCode::Constant.repr());
                output.push(*index);
            }
            Instruction::Negate => output.push(OpCode::Negate.repr()),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Instruction::Return | Instruction::Negate => 1,
            Instruction::Constant(_) => 2,
        }
    }

    pub fn get_opcode(&self) -> OpCode {
        match self {
            Instruction::Return => OpCode::Return,
            Instruction::Constant(_) => OpCode::Constant,
            Instruction::Negate => OpCode::Negate,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "RET"),
            Instruction::Constant(index) => write!(f, "LDC {:4}", *index),
            Instruction::Negate => write!(f, "NEG"),
        }
    }
}

#[EnumRepr(type = "u8")]
#[derive(Debug)]
pub enum OpCode {
    Return = 0,
    Constant = 1,
    Negate = 2,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpCode::Return => write!(f, "Return"),
            OpCode::Constant => write!(f, "Constant"),
            OpCode::Negate => write!(f, "Negate"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::chunk::{Chunk, Instruction};
    use crate::value::Value;

    #[test]
    fn test() {
        let mut chunk = Chunk::new();
        chunk.add_constant(Value::Double(1.2));
        chunk.add_instruction(&Instruction::Constant(0), 1);
        chunk.add_instruction(&Instruction::Return, 1);
        println!("{}", chunk);
    }
}
