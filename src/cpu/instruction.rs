use super::micro_operation::{MicroOperation, NO_OP};
use std::error::Error;
use std::io::{Bytes, Read};

macro_rules! instruction {
    ($opcode: expr, $name: expr, $operations: expr) => {
        Instruction {
            opcode: $opcode as u32,
            name: $name,
            operations: $operations,
        }
    };
}

/// Represents a single Z80 instruction (machine cycle granularity).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction<'a> {
    pub opcode: u32,
    pub name: &'static str,
    pub operations: &'a [MicroOperation],
}

impl<'a> Instruction<'a> {
    pub const NOP: Instruction<'a> = instruction!(0x00, "NOP", &[NO_OP]);

    /// Decodes a single instruction (opcode and operands).
    pub fn decode<R: Read>(bytes: &mut Bytes<R>) -> Option<Self> {
        /// Flattens the return value of the next byte from the iterator to an `Option<u8>`.
        /// Any read error (due to having reached the end of the stream or otherwise) returns `None`.
        fn flatten<E: Error>(byte: Option<Result<u8, E>>) -> Option<u8> {
            byte.and_then(|b| b.ok())
        }

        if let Some(opcode) = flatten(bytes.next()) {
            return match opcode {
                0x00 => Some(Instruction::NOP),
                _ => unimplemented!(),
            };
        } else {
            return None;
        }
    }

    /// Decodes a sequence of instructions, until the end of the stream or an error is reached.
    pub fn decode_all<R: Read>(bytes: &mut Bytes<R>) -> Vec<Instruction> {
        let mut instructions = Vec::new();

        while let Some(instruction) = Instruction::decode(bytes) {
            instructions.push(instruction);
        }

        instructions
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

    #[test]
    fn decode_instruction_sequence() {
        let nop_opcodes = &mut [0x00, 0x00, 0x00].bytes();
        let mut nop_sequence = Instruction::decode_all(nop_opcodes);
        assert_eq!(3, nop_sequence.len());

        while let Some(nop) = nop_sequence.pop() {
            assert_eq!(Instruction::NOP, nop);
        }
    }
}
