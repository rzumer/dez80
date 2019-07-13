use super::storage::{RegisterPairType, RegisterType};
use std::error::Error;
use std::io::{Bytes, Read};

macro_rules! instruction {
    ($name: expr, $operation: expr, $cycles: expr, $source: ident, $dest: ident) => {
        Instruction {
            name: $name,
            operation: $operation,
            cycles: $cycles,
            source: $source,
            destination: $dest,
        }
    };
    ($name: expr, $operation: expr, $cycles: expr) => {
        Instruction {
            name: $name,
            operation: $operation,
            cycles: $cycles,
            source: None,
            destination: None,
        }
    };
}

/// Represents a type of operation that, in combination with data locations,
/// informs its concrete implementation on the CPU.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operation {
    None,
}

/// Represents a target for CPU data operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DataLocation {
    Register(RegisterType),
    RegisterPair(RegisterPairType),
    Memory(u16),
}

/// Represents a single Z80 instruction (machine cycle granularity).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    pub name: &'static str,
    pub operation: Operation,
    pub cycles: usize,
    pub source: Option<DataLocation>,
    pub destination: Option<DataLocation>,
}

impl Instruction {
    pub const NOP: Instruction = instruction!("NOP", Operation::None, 4);

    /// Decodes a single instruction (opcode and operands).
    pub fn decode<R: Read>(reader: &mut Bytes<R>) -> Option<Self> {
        /// Flattens the return value of the next byte from the iterator to an Option<u8>.
        fn flatten<E: Error>(byte: Option<Result<u8, E>>) -> Option<u8> {
            byte.and_then(|b| b.ok())
        }

        if let Some(opcode) = flatten(reader.next()) {
            return match opcode {
                0x00 => Some(Instruction::NOP),
                _ => unimplemented!(),
            };
        } else {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_instruction() {
        let nop = Instruction::decode(&mut [0x00].bytes());
        assert_eq!(Instruction::NOP, nop.unwrap());
    }
}
