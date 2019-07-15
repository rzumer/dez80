use super::micro_operation::*;
use super::storage::*;
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

/// Represents a condition that must be true for a micro-operation to run.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Condition {
    FlagSet(Flag),
    FlagNotSet(Flag),
    RegisterZero(RegisterType),
    RegisterPairZero(RegisterPairType),
}

impl Condition {
    pub fn evaluate(self, registers: &RegisterSet) -> bool {
        match self {
            Condition::FlagSet(f) => registers.flags().flag(f),
            Condition::FlagNotSet(f) => !registers.flags().flag(f),
            Condition::RegisterZero(r) => registers.read(r) == 0,
            Condition::RegisterPairZero(r) => registers.read_pair(r) == 0,
        }
    }
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
    /// Formats instruction types as their canonical mnemonics,
    /// based on the notation employed in the µPD780 data sheet.
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

/// Represents a target for data operations.
/// Variants are closely related to Z80 addressing modes.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operand {
    OctetImmediate(u8),
    DoubletImmediate(u16),
    RegisterImplied(RegisterType),
    RegisterPairImplied(RegisterPairType),
    RegisterBitImplied(RegisterType, u8),
    MemoryDirect(u16),
    MemoryRelative(i8),
    MemoryIndirect(RegisterPairType),
    MemoryIndexed(RegisterPairType, i8),
    MemoryZeroPage(u8),
    MemoryBitIndirect(RegisterPairType, u8),
    MemoryBitIndexed(RegisterPairType, i8, u8),
    PortDirect(u8),
    PortIndirect(RegisterType),
}

impl fmt::Display for Operand {
    /// Formats operands based on the notation employed in the µPD780 data sheet.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Operand::*;

        let formatted = match self {
            OctetImmediate(val) => format!("{:02x}", val),
            DoubletImmediate(val) => format!("{:04x}", val),
            RegisterImplied(reg) => format!("{}", reg),
            RegisterPairImplied(reg) => format!("{}", reg),
            RegisterBitImplied(reg, bit) => format!("{}, {}", bit, reg),
            MemoryDirect(val) => format!("({:04x})", val.to_le()),
            MemoryRelative(val) => format!("{:02x}", val),
            MemoryIndirect(reg) => format!("({})", reg),
            MemoryIndexed(reg, idx) => format!("({} + {:02x})", reg, idx),
            MemoryZeroPage(val) => format!("{:02x}", val),
            MemoryBitIndirect(reg, bit) => format!("{}, ({})", bit, reg),
            MemoryBitIndexed(reg, idx, bit) => format!("{}, ({} + {:02x})", bit, reg, idx),
            PortDirect(val) => format!("({:02x})", val),
            PortIndirect(reg) => format!("({})", reg),
        };

        write!(f, "{}", formatted)
    }
}

/// Represents a single Z80 instruction (machine cycle granularity).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    pub r#type: InstructionType,
    pub source: Option<Operand>,
    pub destination: Option<Operand>,
}

impl Instruction {
    pub const NOP: Instruction =
        Instruction { r#type: InstructionType::Nop, source: None, destination: None };

    fn new(r#type: InstructionType, source: Option<Operand>, destination: Option<Operand>) -> Self {
        Instruction { r#type, source, destination }
    }

    fn operations(&self) -> Vec<MicroOperation> {
        use InstructionType::*;
        use MicroOperationType::*;
        use Operand::*;

        macro_rules! single_operation {
            ($type: expr, $cycles: expr) => {
                vec![micro_op!($type, $cycles, self.source, self.destination)]
            };
        }

        return match self.r#type {
            Ld => match (self.source, self.destination) {
                (Some(DoubletImmediate(_)), Some(RegisterPairImplied(_))) => {
                    single_operation!(Load, 10)
                }
                (Some(RegisterImplied(_)), Some(MemoryIndirect(_))) => single_operation!(Load, 7),
                _ => unimplemented!(),
            },
            Nop => vec![NO_OP],
            _ => unimplemented!(),
        };
    }

    /// Decodes a single instruction (opcode and operands).
    pub fn decode<R: Read>(reader: &mut R) -> Option<Self> {
        let bytes = &mut reader.bytes();

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
                0x00 => Instruction::NOP,
                0x01 => instruction!(
                    Ld,
                    DoubletImmediate(next_doublet(bytes)?),
                    RegisterPairImplied(BC)
                ),
                0x02 => instruction!(Ld, RegisterImplied(A), MemoryIndirect(BC)),
                _ => unimplemented!(),
            };

            Some(instruction)
        } else {
            None
        }
    }

    /// Decodes a sequence of instructions, until the end of the stream or an error is reached.
    pub fn decode_all<R: Read>(reader: &mut R) -> Vec<Instruction> {
        let mut instructions = Vec::new();

        while let Some(instruction) = Instruction::decode(reader) {
            instructions.push(instruction);
        }

        instructions
    }
}

/// Formats instructions by disassembling their raw byte representation.
impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstructionType::*;

        let formatted = match (self.source, self.destination) {
            (Some(src), Some(dst)) => format!("{} {}, {}", self.r#type, dst, src),
            (Some(operand), None) | (None, Some(operand)) => match self.r#type {
                // Conditional instructions are written with a separator
                // between the condition and the operand.
                Call(Some(_)) | Jp(Some(_)) | Jr(Some(_)) | Ret(Some(_)) => {
                    format!("{}, {}", self.r#type, operand)
                }
                _ => format!("{} {}", self.r#type, operand),
            },
            (None, None) => format!("{}", self.r#type),
        };

        write!(f, "{}", formatted)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_instruction() {
        let nop = Instruction::decode(&mut [0x00].as_ref()).unwrap();
        assert_eq!(Instruction::NOP, nop);
    }

    #[test]
    fn decode_instruction_operation() {
        let operation = Instruction::NOP.operations();
        assert_eq!(1, operation.len());
        assert_eq!(NO_OP, operation[0]);
    }

    #[test]
    fn display_instruction() {
        let instruction_bytes = &mut [0x01, 0xF0, 0x0F].as_ref();
        let instruction = Instruction::decode(instruction_bytes).unwrap();
        assert_eq!("LD BC, 0ff0", instruction.to_string());
    }

    #[test]
    fn decode_instruction_sequence() {
        let nop_opcodes = &mut [0x00, 0x00, 0x00].as_ref();
        let mut nop_sequence = Instruction::decode_all(nop_opcodes);
        assert_eq!(3, nop_sequence.len());

        while let Some(nop) = nop_sequence.pop() {
            assert_eq!(Instruction::NOP, nop);
        }
    }
}
