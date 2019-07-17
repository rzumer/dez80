use crate::common::{Condition, Operand};
use crate::operation::*;
use crate::register::*;
use std::fmt;
use std::io::{Bytes, Read};

macro_rules! instruction {
    ($type: expr) => {
        Instruction::new($type, None, None)
    };
    ($type: expr, source: $source: expr) => {
        Instruction::new($type, Some($source), None)
    };
    ($type: expr, destination: $destination: expr) => {
        Instruction::new($type, None, Some($destination))
    };
    ($type: expr, $source: expr, $destination: expr) => {
        Instruction::new($type, Some($source), Some($destination))
    };
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InstructionType {
    Adc,
    Add,
    And,
    Bit,
    Call(Option<Condition>),
    Ccf,
    Cp,
    Cpd,
    Cpdr,
    Cpi,
    Cpir,
    Cpl,
    Daa,
    Dec,
    Di,
    Djnz,
    Ei,
    Ex,
    Exx,
    Halt,
    Im(u8),
    In,
    Inc,
    Ind,
    Indr,
    Ini,
    Inir,
    Jp(Option<Condition>),
    Jr(Option<Condition>),
    Ld,
    Ldd,
    Lddr,
    Ldi,
    Ldir,
    Neg,
    Nop,
    Or,
    Otdr,
    Out,
    Outd,
    Outi,
    Pop,
    Push,
    Res,
    Ret(Option<Condition>),
    Reti,
    Retn,
    Rl,
    Rla,
    Rlc,
    Rlca,
    Rld,
    Rr,
    Rra,
    Rrc,
    Rrca,
    Rrd,
    Rst(u8),
    Sbc,
    Scf,
    Set,
    Sla,
    Sra,
    Srl,
    Sub,
    Xor,
}

impl fmt::Display for InstructionType {
    /// Formats instruction types as their canonical mnemonics.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstructionType::*;

        let formatted = match self {
            Im(val) => format!("{:?} {}", self, val),
            Call(Some(cond)) | Jp(Some(cond)) | Jr(Some(cond)) | Ret(Some(cond)) => match cond {
                Condition::FlagSet(flag) => match flag {
                    Flag::PV => "PO".to_string(), // Parity Odd
                    Flag::S => "P".to_string(),   // Sign Positive
                    _ => format!("{:?} {:?}", self, flag),
                },
                Condition::FlagNotSet(flag) => match flag {
                    Flag::PV => "PE".to_string(), // Parity Even
                    Flag::S => "M".to_string(),   // Sign Negative
                    _ => format!("{:?} N{:?}", self, flag),
                },
                _ => format!("{:?}", self),
            },
            Rst(val) => format!("{:?} {:02X}", self, val),
            _ => format!("{:?}", self),
        };

        // Write out the capitalized enum variant name.
        write!(f, "{}", formatted.to_ascii_uppercase())
    }
}

/// Represents a single Z80 instruction with machine cycle granularity.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    pub r#type: InstructionType,
    pub source: Option<Operand>,
    pub destination: Option<Operand>,
}

impl Instruction {
    fn new(r#type: InstructionType, source: Option<Operand>, destination: Option<Operand>) -> Self {
        Instruction { r#type, source, destination }
    }

    /// Breaks down an instruction into a sequence of operations
    /// providing data relevant to their execution.
    pub fn operations(&self) -> Vec<Operation> {
        use InstructionType::*;
        use Operand::*;
        use OperationType::*;

        macro_rules! single_operation {
            ($type: expr, $cycles: expr) => {
                vec![operation!($type, $cycles, self.source, self.destination)]
            };
        }

        let operands = (self.source, self.destination);

        return match self.r#type {
            InstructionType::Add => {
                let cycles = match operands {
                    (Some(RegisterImplied(_)), Some(RegisterImplied(_))) => 4,
                    (Some(MemoryIndirect(_)), Some(RegisterImplied(_))) => 7,
                    (Some(RegisterPairImplied(_)), Some(RegisterPairImplied(_))) => 11,
                    _ => unimplemented!(),
                };
                single_operation!(OperationType::Add, cycles)
            }
            Dec => {
                let cycles = match self.destination {
                    Some(RegisterImplied(_)) => 4,
                    Some(RegisterPairImplied(_)) => 6,
                    _ => unimplemented!(),
                };
                single_operation!(Decrement, cycles)
            }
            Ex => single_operation!(Exchange, 4),
            Inc => {
                let cycles = match self.destination {
                    Some(RegisterImplied(_)) => 4,
                    Some(RegisterPairImplied(_)) => 6,
                    _ => unimplemented!(),
                };
                single_operation!(Increment, cycles)
            }
            Ld => {
                let cycles = match operands {
                    (Some(DoubletImmediate(_)), Some(RegisterPairImplied(_))) => 10,
                    (Some(RegisterImplied(_)), Some(MemoryIndirect(_)))
                    | (Some(MemoryIndirect(_)), Some(RegisterImplied(_)))
                    | (Some(OctetImmediate(_)), Some(RegisterImplied(_))) => 7,
                    _ => unimplemented!(),
                };
                single_operation!(Load, cycles)
            }
            Nop => vec![NO_OP],
            Rlca => single_operation!(RotateLeftThroughCarry, 4),
            Rrca => single_operation!(RotateRightThroughCarry, 4),
            _ => unimplemented!(),
        };
    }

    /// Decodes a single instruction (opcode and operands).
    fn decode<R: Read>(bytes: &mut Bytes<R>) -> Option<Self> {
        /// Flattens the next byte in the stream to an `Option<u8>` value.
        /// Any read error (due to having reached the end of the stream or otherwise) returns `None`.
        fn next_byte<R: Read>(bytes: &mut Bytes<R>) -> Option<u8> {
            bytes.next().and_then(|b| b.ok())
        }

        /// Flattens the next two bytes in the stream to an `Option<u16>` value.
        /// Any read error (due to having reached the end of the stream or otherwise) returns `None`.
        fn next_doublet<R: Read>(bytes: &mut Bytes<R>) -> Option<u16> {
            Some(u16::from_le_bytes([next_byte(bytes)?, next_byte(bytes)?]))
        }

        if let Some(opcode) = next_byte(bytes) {
            use InstructionType::*;
            use Operand::*;
            use RegisterPairType::*;
            use RegisterType::*;

            let instruction = match opcode {
                0x00 => instruction!(Nop),
                0x01 => instruction!(
                    Ld,
                    DoubletImmediate(next_doublet(bytes)?),
                    RegisterPairImplied(BC)
                ),
                0x02 => instruction!(Ld, RegisterImplied(A), MemoryIndirect(BC)),
                0x03 => instruction!(Inc, destination: RegisterPairImplied(BC)),
                0x04 => instruction!(Inc, destination: RegisterImplied(B)),
                0x05 => instruction!(Dec, destination: RegisterImplied(B)),
                0x06 => instruction!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(B)),
                0x07 => instruction!(Rlca),
                0x08 => instruction!(Ex, RegisterPairImplied(AF_), RegisterPairImplied(AF)),
                0x09 => instruction!(Add, RegisterPairImplied(BC), RegisterPairImplied(HL)),
                0x0A => instruction!(Ld, MemoryIndirect(BC), RegisterImplied(A)),
                0x0B => instruction!(Dec, destination: RegisterPairImplied(BC)),
                0x0C => instruction!(Inc, destination: RegisterImplied(C)),
                0x0D => instruction!(Dec, destination: RegisterImplied(C)),
                0x0E => instruction!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(C)),
                0x0F => instruction!(Rrca),
                _ => unimplemented!(),
            };

            Some(instruction)
        } else {
            None
        }
    }

    /// Decodes a sequence of instructions, until the end of the stream or an error is reached.
    pub fn from_bytes<R: Read>(reader: &mut R) -> Vec<Instruction> {
        let mut bytes = reader.bytes();
        let mut instructions = Vec::new();

        while let Some(instruction) = Instruction::decode(&mut bytes) {
            instructions.push(instruction);
        }

        instructions
    }
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction::decode(&mut [0x00].bytes()).unwrap()
    }
}

/// Formats instructions by disassembling their raw byte representation.
impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstructionType::*;

        match (self.source, self.destination) {
            (Some(src), Some(dst)) => write!(f, "{} {}, {}", self.r#type, dst, src),
            (Some(operand), None) | (None, Some(operand)) => match self.r#type {
                // Conditional instructions are written with a separator
                // between the condition and the operand.
                Call(Some(_)) | Jp(Some(_)) | Jr(Some(_)) | Ret(Some(_)) => {
                    write!(f, "{}, {}", self.r#type, operand)
                }
                _ => write!(f, "{} {}", self.r#type, operand),
            },
            (None, None) => write!(f, "{}", self.r#type),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_instruction() {
        let inc_b = Instruction::decode(&mut [0x04].bytes()).unwrap();
        assert_eq!(None, inc_b.source);
        assert_eq!(Some(Operand::RegisterImplied(RegisterType::B)), inc_b.destination);
        assert_eq!(InstructionType::Inc, inc_b.r#type);
    }

    #[test]
    fn decode_incomplete_instruction() {
        let ld_b = Instruction::decode(&mut [0x06].bytes());
        assert_eq!(None, ld_b);
    }

    #[test]
    fn decode_instruction_default() {
        let nop = Instruction::decode(&mut [0x00].bytes()).unwrap();
        assert_eq!(Instruction::default(), nop);
    }

    #[test]
    fn decode_instruction_operation() {
        let nop_operations = Instruction::default().operations();
        assert_eq!(1, nop_operations.len());
        assert_eq!(NO_OP, nop_operations[0]);
    }

    #[test]
    fn display_instruction() {
        let ld_bc_bytes = &mut [0x01, 0xF0, 0x0F].bytes();
        let ld_bc = Instruction::decode(ld_bc_bytes).unwrap();
        assert_eq!("LD BC, 0ff0", ld_bc.to_string());
    }

    #[test]
    fn decode_instruction_sequence() {
        let nop_sequence_bytes = &mut [0x00, 0x00, 0x00].as_ref();
        let mut nop_sequence = Instruction::from_bytes(nop_sequence_bytes);
        assert_eq!(3, nop_sequence.len());

        while let Some(nop) = nop_sequence.pop() {
            assert_eq!(Instruction::default(), nop);
        }
    }

    #[test]
    fn decode_incomplete_instruction_sequence() {
        let instruction_sequence_bytes = &mut [0x00, 0x00, 0x06].as_ref();
        let mut instruction_sequence = Instruction::from_bytes(instruction_sequence_bytes);

        assert_eq!(2, instruction_sequence.len());

        while let Some(nop) = instruction_sequence.pop() {
            assert_eq!(Instruction::default(), nop);
        }
    }
}
