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

        match self.r#type {
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
        }
    }

    /// Decodes a single instruction (opcode and operands).
    #[allow(clippy::cognitive_complexity)]
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

        // Parse the instruction opcode and operands byte by byte.
        use Condition::*;
        use InstructionType::*;
        use Operand::*;
        use RegisterPairType::*;
        use RegisterType::*;

        #[rustfmt::skip]
        let instruction = match next_byte(bytes)? {
            0x00 => instruction!(Nop),
            0x01 => instruction!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(BC)),
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
            0x10 => instruction!(Djnz, source: OctetImmediate(next_byte(bytes)?)),
            0x11 => instruction!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(DE)),
            0x12 => instruction!(Ld, RegisterImplied(A), MemoryIndirect(DE)),
            0x13 => instruction!(Inc, destination: RegisterPairImplied(DE)),
            0x14 => instruction!(Inc, destination: RegisterImplied(D)),
            0x15 => instruction!(Dec, destination: RegisterImplied(D)),
            0x16 => instruction!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(D)),
            0x17 => instruction!(Rla),
            0x18 => instruction!(Jr(None), source: OctetImmediate(next_byte(bytes)?)),
            0x19 => instruction!(Add, RegisterPairImplied(DE), RegisterPairImplied(HL)),
            0x1A => instruction!(Ld, MemoryIndirect(DE), RegisterImplied(A)),
            0x1B => instruction!(Dec, destination: RegisterPairImplied(DE)),
            0x1C => instruction!(Inc, destination: RegisterImplied(E)),
            0x1D => instruction!(Dec, destination: RegisterImplied(E)),
            0x1E => instruction!(Ld, RegisterImplied(E), OctetImmediate(next_byte(bytes)?)),
            0x1F => instruction!(Rra),
            0x20 => instruction!(Jr(Some(FlagNotSet(Flag::Z))), source: OctetImmediate(next_byte(bytes)?)),
            0x21 => instruction!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(HL)),
            0x22 => instruction!(Ld, RegisterPairImplied(HL), MemoryDirect(next_doublet(bytes)?)),
            0x23 => instruction!(Inc, destination: RegisterPairImplied(HL)),
            0x24 => instruction!(Inc, destination: RegisterImplied(H)),
            0x25 => instruction!(Dec, destination: RegisterImplied(H)),
            0x26 => instruction!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(H)),
            0x27 => instruction!(Daa),
            0x28 => instruction!(Jr(Some(FlagSet(Flag::Z))), source: OctetImmediate(next_byte(bytes)?)),
            0x29 => instruction!(Add, RegisterPairImplied(HL), RegisterPairImplied(HL)),
            0x2A => instruction!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(HL)),
            0x2B => instruction!(Dec, destination: RegisterPairImplied(HL)),
            0x2C => instruction!(Inc, destination: RegisterImplied(L)),
            0x2D => instruction!(Dec, destination: RegisterImplied(L)),
            0x2E => instruction!(Ld, RegisterImplied(L), OctetImmediate(next_byte(bytes)?)),
            0x2F => instruction!(Cpl),
            0x30 => instruction!(Jr(Some(FlagNotSet(Flag::C))), source: OctetImmediate(next_byte(bytes)?)),
            0x31 => instruction!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(SP)),
            0x32 => instruction!(Ld, RegisterImplied(A), MemoryDirect(next_doublet(bytes)?)),
            0x33 => instruction!(Inc, destination: RegisterPairImplied(SP)),
            0x34 => instruction!(Inc, destination: MemoryIndirect(HL)),
            0x35 => instruction!(Dec, destination: MemoryIndirect(HL)),
            0x36 => instruction!(Ld, OctetImmediate(next_byte(bytes)?), MemoryIndirect(HL)),
            0x37 => instruction!(Scf),
            0x38 => instruction!(Jr(Some(FlagSet(Flag::C))), source: OctetImmediate(next_byte(bytes)?)),
            0x39 => instruction!(Add, RegisterPairImplied(SP), RegisterPairImplied(HL)),
            0x3A => instruction!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterImplied(A)),
            0x3B => instruction!(Dec, destination: RegisterPairImplied(SP)),
            0x3C => instruction!(Inc, destination: RegisterImplied(A)),
            0x3D => instruction!(Dec, destination: RegisterImplied(A)),
            0x3E => instruction!(Ld, RegisterImplied(A), OctetImmediate(next_byte(bytes)?)),
            0x3F => instruction!(Ccf),
            0x40 => instruction!(Ld, RegisterImplied(B), RegisterImplied(B)),
            0x41 => instruction!(Ld, RegisterImplied(C), RegisterImplied(B)),
            0x42 => instruction!(Ld, RegisterImplied(D), RegisterImplied(B)),
            0x43 => instruction!(Ld, RegisterImplied(E), RegisterImplied(B)),
            0x44 => instruction!(Ld, RegisterImplied(H), RegisterImplied(B)),
            0x45 => instruction!(Ld, RegisterImplied(L), RegisterImplied(B)),
            0x46 => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(B)),
            0x47 => instruction!(Ld, RegisterImplied(A), RegisterImplied(C)),
            0x48 => instruction!(Ld, RegisterImplied(B), RegisterImplied(C)),
            0x49 => instruction!(Ld, RegisterImplied(C), RegisterImplied(C)),
            0x4A => instruction!(Ld, RegisterImplied(D), RegisterImplied(C)),
            0x4B => instruction!(Ld, RegisterImplied(E), RegisterImplied(C)),
            0x4C => instruction!(Ld, RegisterImplied(H), RegisterImplied(C)),
            0x4D => instruction!(Ld, RegisterImplied(L), RegisterImplied(C)),
            0x4E => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(C)),
            0x4F => instruction!(Ld, RegisterImplied(A), RegisterImplied(C)),
            0x50 => instruction!(Ld, RegisterImplied(B), RegisterImplied(D)),
            0x51 => instruction!(Ld, RegisterImplied(C), RegisterImplied(D)),
            0x52 => instruction!(Ld, RegisterImplied(D), RegisterImplied(D)),
            0x53 => instruction!(Ld, RegisterImplied(E), RegisterImplied(D)),
            0x54 => instruction!(Ld, RegisterImplied(H), RegisterImplied(D)),
            0x55 => instruction!(Ld, RegisterImplied(L), RegisterImplied(D)),
            0x56 => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(D)),
            0x57 => instruction!(Ld, RegisterImplied(A), RegisterImplied(D)),
            0x58 => instruction!(Ld, RegisterImplied(B), RegisterImplied(E)),
            0x59 => instruction!(Ld, RegisterImplied(C), RegisterImplied(E)),
            0x5A => instruction!(Ld, RegisterImplied(D), RegisterImplied(E)),
            0x5B => instruction!(Ld, RegisterImplied(E), RegisterImplied(E)),
            0x5C => instruction!(Ld, RegisterImplied(H), RegisterImplied(E)),
            0x5D => instruction!(Ld, RegisterImplied(L), RegisterImplied(E)),
            0x5E => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(E)),
            0x5F => instruction!(Ld, RegisterImplied(A), RegisterImplied(E)),
            0x60 => instruction!(Ld, RegisterImplied(B), RegisterImplied(H)),
            0x61 => instruction!(Ld, RegisterImplied(C), RegisterImplied(H)),
            0x62 => instruction!(Ld, RegisterImplied(D), RegisterImplied(H)),
            0x63 => instruction!(Ld, RegisterImplied(E), RegisterImplied(H)),
            0x64 => instruction!(Ld, RegisterImplied(H), RegisterImplied(H)),
            0x65 => instruction!(Ld, RegisterImplied(L), RegisterImplied(H)),
            0x66 => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(H)),
            0x67 => instruction!(Ld, RegisterImplied(A), RegisterImplied(H)),
            0x68 => instruction!(Ld, RegisterImplied(B), RegisterImplied(L)),
            0x69 => instruction!(Ld, RegisterImplied(C), RegisterImplied(L)),
            0x6A => instruction!(Ld, RegisterImplied(D), RegisterImplied(L)),
            0x6B => instruction!(Ld, RegisterImplied(E), RegisterImplied(L)),
            0x6C => instruction!(Ld, RegisterImplied(H), RegisterImplied(L)),
            0x6D => instruction!(Ld, RegisterImplied(L), RegisterImplied(L)),
            0x6E => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(L)),
            0x6F => instruction!(Ld, RegisterImplied(A), RegisterImplied(L)),
            0x70 => instruction!(Ld, RegisterImplied(B), MemoryIndirect(HL)),
            0x71 => instruction!(Ld, RegisterImplied(C), MemoryIndirect(HL)),
            0x72 => instruction!(Ld, RegisterImplied(D), MemoryIndirect(HL)),
            0x73 => instruction!(Ld, RegisterImplied(E), MemoryIndirect(HL)),
            0x74 => instruction!(Ld, RegisterImplied(H), MemoryIndirect(HL)),
            0x75 => instruction!(Ld, RegisterImplied(L), MemoryIndirect(HL)),
            0x76 => instruction!(Halt),
            0x77 => instruction!(Ld, RegisterImplied(A), MemoryIndirect(HL)),
            0x78 => instruction!(Ld, RegisterImplied(B), RegisterImplied(A)),
            0x79 => instruction!(Ld, RegisterImplied(C), RegisterImplied(A)),
            0x7A => instruction!(Ld, RegisterImplied(D), RegisterImplied(A)),
            0x7B => instruction!(Ld, RegisterImplied(E), RegisterImplied(A)),
            0x7C => instruction!(Ld, RegisterImplied(H), RegisterImplied(A)),
            0x7D => instruction!(Ld, RegisterImplied(L), RegisterImplied(A)),
            0x7E => instruction!(Ld, MemoryIndirect(HL), RegisterImplied(A)),
            0x7F => instruction!(Ld, RegisterImplied(A), RegisterImplied(A)),
            0x80 => instruction!(Add, RegisterImplied(B), RegisterImplied(A)),
            0x81 => instruction!(Add, RegisterImplied(C), RegisterImplied(A)),
            0x82 => instruction!(Add, RegisterImplied(D), RegisterImplied(A)),
            0x83 => instruction!(Add, RegisterImplied(E), RegisterImplied(A)),
            0x84 => instruction!(Add, RegisterImplied(H), RegisterImplied(A)),
            0x85 => instruction!(Add, RegisterImplied(L), RegisterImplied(A)),
            0x86 => instruction!(Add, MemoryIndirect(HL), RegisterImplied(A)),
            0x87 => instruction!(Add, RegisterImplied(A), RegisterImplied(A)),
            0x88 => instruction!(Adc, RegisterImplied(B), RegisterImplied(A)),
            0x89 => instruction!(Adc, RegisterImplied(C), RegisterImplied(A)),
            0x8A => instruction!(Adc, RegisterImplied(D), RegisterImplied(A)),
            0x8B => instruction!(Adc, RegisterImplied(E), RegisterImplied(A)),
            0x8C => instruction!(Adc, RegisterImplied(H), RegisterImplied(A)),
            0x8D => instruction!(Adc, RegisterImplied(L), RegisterImplied(A)),
            0x8E => instruction!(Adc, MemoryIndirect(HL), RegisterImplied(A)),
            0x8F => instruction!(Adc, RegisterImplied(A), RegisterImplied(A)),
            0x90 => instruction!(Sub, destination: RegisterImplied(B)),
            0x91 => instruction!(Sub, destination: RegisterImplied(C)),
            0x92 => instruction!(Sub, destination: RegisterImplied(D)),
            0x93 => instruction!(Sub, destination: RegisterImplied(E)),
            0x94 => instruction!(Sub, destination: RegisterImplied(H)),
            0x95 => instruction!(Sub, destination: RegisterImplied(L)),
            0x96 => instruction!(Sub, destination: MemoryIndirect(HL)),
            0x97 => instruction!(Sub, destination: RegisterImplied(A)),
            0x98 => instruction!(Sbc, RegisterImplied(B), RegisterImplied(A)),
            0x99 => instruction!(Sbc, RegisterImplied(C), RegisterImplied(A)),
            0x9A => instruction!(Sbc, RegisterImplied(D), RegisterImplied(A)),
            0x9B => instruction!(Sbc, RegisterImplied(E), RegisterImplied(A)),
            0x9C => instruction!(Sbc, RegisterImplied(H), RegisterImplied(A)),
            0x9D => instruction!(Sbc, RegisterImplied(L), RegisterImplied(A)),
            0x9E => instruction!(Sbc, MemoryIndirect(HL), RegisterImplied(A)),
            0x9F => instruction!(Sbc, RegisterImplied(A), RegisterImplied(A)),
            0xA0 => instruction!(And, destination: RegisterImplied(B)),
            0xA1 => instruction!(And, destination: RegisterImplied(C)),
            0xA2 => instruction!(And, destination: RegisterImplied(D)),
            0xA3 => instruction!(And, destination: RegisterImplied(E)),
            0xA4 => instruction!(And, destination: RegisterImplied(H)),
            0xA5 => instruction!(And, destination: RegisterImplied(L)),
            0xA6 => instruction!(And, destination: MemoryIndirect(HL)),
            0xA7 => instruction!(And, destination: RegisterImplied(A)),
            0xA8 => instruction!(Xor, destination: RegisterImplied(B)),
            0xA9 => instruction!(Xor, destination: RegisterImplied(C)),
            0xAA => instruction!(Xor, destination: RegisterImplied(D)),
            0xAB => instruction!(Xor, destination: RegisterImplied(E)),
            0xAC => instruction!(Xor, destination: RegisterImplied(H)),
            0xAD => instruction!(Xor, destination: RegisterImplied(L)),
            0xAE => instruction!(Xor, destination: MemoryIndirect(HL)),
            0xAF => instruction!(Xor, destination: RegisterImplied(A)),
            0xB0 => instruction!(Or, destination: RegisterImplied(B)),
            0xB1 => instruction!(Or, destination: RegisterImplied(C)),
            0xB2 => instruction!(Or, destination: RegisterImplied(D)),
            0xB3 => instruction!(Or, destination: RegisterImplied(E)),
            0xB4 => instruction!(Or, destination: RegisterImplied(H)),
            0xB5 => instruction!(Or, destination: RegisterImplied(L)),
            0xB6 => instruction!(Or, destination: MemoryIndirect(HL)),
            0xB7 => instruction!(Or, destination: RegisterImplied(A)),
            0xB8 => instruction!(Cp, destination: RegisterImplied(B)),
            0xB9 => instruction!(Cp, destination: RegisterImplied(C)),
            0xBA => instruction!(Cp, destination: RegisterImplied(D)),
            0xBB => instruction!(Cp, destination: RegisterImplied(E)),
            0xBC => instruction!(Cp, destination: RegisterImplied(H)),
            0xBD => instruction!(Cp, destination: RegisterImplied(L)),
            0xBE => instruction!(Cp, destination: MemoryIndirect(HL)),
            0xBF => instruction!(Cp, destination: RegisterImplied(A)),
            0xC0 => instruction!(Ret(Some(FlagNotSet(Flag::Z)))),
            0xC1 => instruction!(Pop, destination: RegisterPairImplied(BC)),
            0xC2 => instruction!(Jp(Some(FlagNotSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xC3 => instruction!(Jp(None), source: DoubletImmediate(next_doublet(bytes)?)),
            0xC4 => instruction!(Call(Some(FlagNotSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xC5 => instruction!(Push, source: RegisterPairImplied(BC)),
            0xC6 => instruction!(Add, OctetImmediate(next_byte(bytes)?), RegisterImplied(A)),
            0xC7 => instruction!(Rst(0x00)),
            0xC8 => instruction!(Ret(Some(FlagSet(Flag::Z)))),
            0xC9 => instruction!(Ret(None)),
            0xCA => instruction!(Jp(Some(FlagSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xCB => unimplemented!("Bit instruction table not yet supported"),
            0xCC => instruction!(Call(Some(FlagSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xCD => instruction!(Call(None), source: DoubletImmediate(next_doublet(bytes)?)),
            0xCE => instruction!(Adc, OctetImmediate(next_byte(bytes)?), RegisterImplied(A)),
            0xCF => instruction!(Rst(0x08)),
            0xD0 => instruction!(Ret(Some(FlagNotSet(Flag::C)))),
            0xD1 => instruction!(Pop, destination: RegisterPairImplied(DE)),
            0xD2 => instruction!(Jp(Some(FlagNotSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xD3 => instruction!(Out, RegisterImplied(A), PortDirect(next_byte(bytes)?)),
            0xD4 => instruction!(Call(Some(FlagNotSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xD5 => instruction!(Push, source: RegisterPairImplied(DE)),
            0xD6 => instruction!(Sub, source: OctetImmediate(next_byte(bytes)?)),
            0xD7 => instruction!(Rst(0x10)),
            0xD8 => instruction!(Ret(Some(FlagSet(Flag::C)))),
            0xD9 => instruction!(Exx),
            0xDA => instruction!(Jp(Some(FlagSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xDB => instruction!(Out, PortDirect(next_byte(bytes)?), RegisterImplied(A)),
            0xDC => instruction!(Call(Some(FlagSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xDD => unimplemented!("IX instruction table not yet supported"),
            0xDE => instruction!(Sbc, OctetImmediate(next_byte(bytes)?), RegisterImplied(A)),
            0xDF => instruction!(Rst(0x18)),
            0xE0 => instruction!(Ret(Some(FlagSet(Flag::PV)))),
            0xE1 => instruction!(Pop, destination: RegisterPairImplied(HL)),
            0xE2 => instruction!(Jp(Some(FlagSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xE3 => instruction!(Ex, RegisterPairImplied(HL), MemoryIndirect(SP)),
            0xE4 => instruction!(Call(Some(FlagSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xE5 => instruction!(Push, source: RegisterPairImplied(HL)),
            0xE6 => instruction!(And, source: OctetImmediate(next_byte(bytes)?)),
            0xE7 => instruction!(Rst(0x20)),
            0xE8 => instruction!(Ret(Some(FlagNotSet(Flag::PV)))),
            0xE9 => instruction!(Jp(None), source: MemoryIndirect(HL)),
            0xEA => instruction!(Jp(Some(FlagNotSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xEB => instruction!(Ex, RegisterPairImplied(HL), RegisterPairImplied(DE)),
            0xEC => instruction!(Call(Some(FlagNotSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xED => match next_byte(bytes)? {
                _ => return None,
            },
            0xEE => instruction!(Xor, source: OctetImmediate(next_byte(bytes)?)),
            0xEF => instruction!(Rst(0x28)),
            0xF0 => instruction!(Ret(Some(FlagSet(Flag::S)))),
            0xF1 => instruction!(Pop, destination: RegisterPairImplied(AF)),
            0xF2 => instruction!(Jp(Some(FlagSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xF3 => instruction!(Di),
            0xF4 => instruction!(Call(Some(FlagSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xF5 => instruction!(Push, source: RegisterPairImplied(AF)),
            0xF6 => instruction!(Or, source: OctetImmediate(next_byte(bytes)?)),
            0xF7 => instruction!(Rst(0x30)),
            0xF8 => instruction!(Ret(Some(FlagNotSet(Flag::S)))),
            0xF9 => instruction!(Ld, RegisterPairImplied(HL), RegisterPairImplied(SP)),
            0xFA => instruction!(Jp(Some(FlagNotSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xFB => instruction!(Ei),
            0xFC => instruction!(Call(Some(FlagNotSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xFD => unimplemented!("IY instruction table not yet supported"),
            0xFE => instruction!(Cp, source: OctetImmediate(next_byte(bytes)?)),
            0xFF => instruction!(Rst(0x38)),
        };

        Some(instruction)
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
        assert_eq!("LD BC, 0x0ff0", ld_bc.to_string());
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
