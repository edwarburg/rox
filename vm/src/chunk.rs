use std::fmt;

use crate::value::Value;
use std::error::Error;

pub type ConstantPoolIndex = u8;
pub const MAX_CONSTANTS: usize = std::u8::MAX as usize;
pub type LineNumber = usize;

pub struct Chunk {
    // TODO this is less efficient than it could be because instructions are as wide as their widest member. Revisit and do the whole byte-packing thing later. KISS for now.
    instructions: Vec<Instruction>,
    constant_pool: Vec<Value>,
    lines: Vec<LineNumber>,
}

#[derive(Debug)]
pub enum ChunkError {
    ConstantPoolOverflow,
}

impl Error for ChunkError {}
impl fmt::Display for ChunkError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            instructions: Vec::new(),
            constant_pool: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> Result<ConstantPoolIndex, ChunkError> {
        if self.constant_pool.len() >= MAX_CONSTANTS {
            return Err(ChunkError::ConstantPoolOverflow);
        }

        let index = self.constant_pool.len();
        self.constant_pool.push(constant);
        Ok(index as ConstantPoolIndex)
    }

    pub fn constants(&self) -> &[Value] {
        &self.constant_pool[..]
    }

    pub fn add_instruction(&mut self, instruction: Instruction, line: LineNumber) {
        self.lines.push(line);
        self.instructions.push(instruction)
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions[..]
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.instructions.is_empty() {
            write!(f, "<empty>");
            return Ok(());
        }
        for (i, inst) in self.instructions.iter().enumerate() {
            write!(f, "{:04} {:4}     {}", i, self.lines[i], inst);

            match inst {
                Instruction::Constant(index) => {
                    write!(f, "    ; value: {}", self.constant_pool[*index as usize])?
                }
                _ => {}
            }
            write!(f, "\n");
        }
        Ok(())
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Chunk (\n{})", self)
    }
}

macro_rules! replace_tt {
    ($_t:tt $sub:tt) => {
        $sub
    };
}

macro_rules! instructions {
    ( $( $name:ident $( ( $( $t:ty ),* ) )? => $size:expr ),* ) => {
        #[derive(Debug, Eq, PartialOrd, PartialEq)]
        pub enum Instruction {
            $(
                $name$(($($t)*))?,
            )*
        }

        impl Instruction {
            pub fn get_opcode(&self) -> OpCode {
                match self {
                    $(
                        Instruction::$name$(($(replace_tt!($t _))*))? => OpCode::$name,
                    )*
                }
            }

            pub fn size(&self) -> usize {
                match self {
                    $(
                        Instruction::$name$(($(replace_tt!($t _))*))? => $size,
                    )*
                }
            }
        }

        #[derive(Debug)]
        pub enum OpCode {
            $(
                $name,
            )*
        }

        impl fmt::Display for OpCode {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $(
                        OpCode::$name => write!(f, "{:?}", self),
                    )*
                }
            }
        }
    };
}

instructions! {
    Return => 1,
    Constant(ConstantPoolIndex) => 2,
    Negate => 1,
    Add => 1,
    Subtract => 1,
    Multiply => 1,
    Divide => 1
}

impl fmt::Display for Instruction {
    // TODO incorporate into macro?
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "RET"),
            Instruction::Constant(index) => write!(f, "LDC {:4}", *index),
            Instruction::Negate => write!(f, "NEG"),
            Instruction::Add => write!(f, "ADD"),
            Instruction::Subtract => write!(f, "SUB"),
            Instruction::Multiply => write!(f, "MUL"),
            Instruction::Divide => write!(f, "DIV"),
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
        chunk.add_instruction(Instruction::Constant(0), 1);
        chunk.add_instruction(Instruction::Return, 1);
        println!("{}", chunk);
    }
}
