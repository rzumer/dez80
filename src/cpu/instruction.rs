use super::micro_operation::{
    DataLocation, MicroOperation, MicroOperationType, NO_OP,
};
use super::storage::{RegisterPairType, RegisterType};
use std::error::Error;
use std::fmt;
use std::io::{Bytes, Read};

macro_rules! instruction {
    ($opcode: expr, $name: expr, $( $operations: expr ),*) => {
        Instruction {
            opcode: u32::from($opcode),
            name: $name,
            operations: vec!($( $operations )*),
        }
    };
}

/// Represents a single Z80 instruction (machine cycle granularity).
#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    pub opcode: u32,
    pub name: String,
    pub operations: Vec<MicroOperation>,
}

impl Instruction {
    /// Decodes a single instruction (opcode and operands).
    pub fn decode<R: Read>(bytes: &mut Bytes<R>) -> Option<Self> {
        /// Flattens the return value of the next byte from the iterator to an `Option<u8>`.
        /// Any read error (due to having reached the end of the stream or otherwise) returns `None`.
        fn flatten<E: Error>(byte: Option<Result<u8, E>>) -> Option<u8> {
            byte.and_then(|b| b.ok())
        }

        /// Flattens the next two bytes in the stream into a `Some(u16)` value,
        /// or `None` if either byte cannot be read.
        fn next_word<R: Read>(bytes: &mut Bytes<R>) -> Option<u16> {
            let word_bytes = (flatten(bytes.next()), flatten(bytes.next()));

            match word_bytes {
                (Some(low), Some(high)) => {
                    Some(u16::from(high) << 8 | u16::from(low))
                }
                _ => None,
            }
        }

        if let Some(opcode) = flatten(bytes.next()) {
            use DataLocation::*;
            use MicroOperationType::*;
            use RegisterPairType::*;
            use RegisterType::*;

            let instruction = match opcode {
                0x00 => instruction!(opcode, "NOP".to_string(), NO_OP),
                0x01 => {
                    let operand = next_word(bytes)?;

                    instruction!(
                        opcode,
                        format!("LD BC, ({:04X})", operand.swap_bytes()),
                        micro_op!(
                            Load,
                            10,
                            MemoryImmediate(operand),
                            RegisterPair(BC)
                        )
                    )
                }
                _ => unimplemented!(),
            };

            Some(instruction)
        } else {
            None
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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_instruction() {
        let nop = Instruction::decode(&mut [0x00].bytes()).unwrap();
        assert_eq!(NO_OP, *nop.operations.first().unwrap());
    }

    #[test]
    fn decode_instruction_with_formatted_name() {
        let instruction_bytes = &mut [0x01, 0xF0, 0x0F].bytes();
        let instruction = Instruction::decode(instruction_bytes).unwrap();
        assert_eq!("LD BC, (F00F)", instruction.name);
    }

    #[test]
    fn decode_instruction_sequence() {
        let nop_opcodes = &mut [0x00, 0x00, 0x00].bytes();
        let mut nop_sequence = Instruction::decode_all(nop_opcodes);
        assert_eq!(3, nop_sequence.len());

        while let Some(nop) = nop_sequence.pop() {
            assert_eq!(NO_OP, *nop.operations.first().unwrap());
        }
    }
}
