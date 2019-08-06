use crate::common::{Condition, Operand};
use crate::operation::*;
use crate::register::*;
use std::fmt;
use std::io::{Bytes, Read};

macro_rules! instruction {
    ($opcode: expr, $type: expr) => {
        Instruction { opcode: $opcode, r#type: $type, source: None, destination: None }
    };
    ($opcode: expr, $type: expr, source: $src: expr) => {
        Instruction { opcode: $opcode, r#type: $type, source: Some($src), destination: None }
    };
    ($opcode: expr, $type: expr, destination: $dst: expr) => {
        Instruction { opcode: $opcode, r#type: $type, source: None, destination: Some($dst) }
    };
    ($opcode: expr, $type: expr, destination: $dst: expr, source: $src: expr) => {
        Instruction { opcode: $opcode, r#type: $type, destination: Some($dst), source: Some($src) }
    };
    ($opcode: expr, $type: expr, $src: expr, $dst: expr) => {
        Instruction { opcode: $opcode, r#type: $type, source: Some($src), destination: Some($dst) }
    };
}

/// Represents a type of instruction, categorized by mnemonic.
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
    Inva, // Unofficial mnemonic, invalid instruction with no observable side effect; equivalent to NONI followed by NOP
    Jp(Option<Condition>),
    Jr(Option<Condition>),
    Ld,
    Ldd,
    Lddr,
    Ldi,
    Ldir,
    Neg,
    Noni, // Unofficial mnemonic, equivalent to NOP but blocks interrupts until the next decode cycle
    Nop,
    Or,
    Otdr,
    Otir,
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
    Sll, // Unofficial mnemonic, also known as SLS and SL1; shift left and pad with 1
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

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum OpcodePrefix {
    Bitwise,                          // 0xCB
    Extended,                         // 0xED
    Indexed(RegisterPairType),        // 0xDD or 0xFD
    IndexedBitwise(RegisterPairType), // 0xDDCB or 0xFDCB
}

impl OpcodePrefix {
    pub fn to_bytes(&self) -> &[u8] {
        use OpcodePrefix::*;
        use RegisterPairType::*;

        match self {
            Bitwise => &[0xCB],
            Extended => &[0xED],
            Indexed(IX) => &[0xDD],
            Indexed(IY) => &[0xFD],
            IndexedBitwise(IX) => &[0xDD, 0xCB],
            IndexedBitwise(IY) => &[0xFD, 0xCB],
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for OpcodePrefix {
    /// Formats the prefix as its raw byte values.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.to_bytes();

        write!(f, "{:02X}", bytes[0])?;

        for byte in &bytes[1..] {
            write!(f, " {:02X}", byte)?;
        }

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Opcode {
    prefix: Option<OpcodePrefix>,
    value: u8,
}

impl fmt::Display for Opcode {
    /// Formats the opcode as a sequence of raw hexadecimal values.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(prefix) = self.prefix {
            write!(f, "{} {:02X}", prefix, self.value)
        } else {
            write!(f, "{:02X}", self.value)
        }
    }
}

/// Represents a single Z80 instruction with machine cycle granularity.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    opcode: Opcode,
    r#type: InstructionType,
    source: Option<Operand>,
    destination: Option<Operand>,
}

impl Instruction {
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

        /// Flattens the next byte in the stream to an `Option<u8>` value.
        /// Any read error (due to having reached the end of the stream or otherwise) returns `None`.
        fn peek_byte<R: Read>(bytes: &mut Bytes<R>) -> Option<u8> {
            if let Some(&Ok(val)) = bytes.peekable().peek() {
                Some(val)
            } else {
                None
            }
        }

        use Condition::*;
        use InstructionType::*;
        use OpcodePrefix::*;
        use Operand::*;
        use RegisterPairType::*;
        use SingleRegisterType::*;

        /// Decodes a partial extended instruction, whose opcode is known to begin with `0xED`.
        #[rustfmt::skip]
        fn decode_extended_instruction<R: Read>(bytes: &mut Bytes<R>) -> Option<Instruction> {
            let opcode = next_byte(bytes)?;

            macro_rules! extended {
                ($($args: tt)+) => { instruction!(Opcode { prefix: Some(Extended), value: opcode }, $($args)+) }
            }

            match opcode {
                // 0x00 ~ 0x3F
                0x40 => extended!(In, PortIndirect(C), RegisterImplied(B)),
                0x41 => extended!(Out, RegisterImplied(B), PortIndirect(C)),
                0x42 => extended!(Sbc, RegisterPairImplied(BC), RegisterPairImplied(HL)),
                0x43 => extended!(Ld, RegisterPairImplied(BC), MemoryDirect(next_doublet(bytes)?)),
                0x44 => extended!(Neg),
                0x45 => extended!(Retn),
                0x46 => extended!(Im(0)),
                0x47 => extended!(Ld, RegisterImplied(A), RegisterImplied(I)),
                0x48 => extended!(In, PortIndirect(C), RegisterImplied(C)),
                0x49 => extended!(Out, RegisterImplied(C), PortIndirect(C)),
                0x4A => extended!(Adc, RegisterPairImplied(BC), RegisterPairImplied(HL)),
                0x4B => extended!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(BC)),
                0x4C => extended!(Neg),
                0x4D => extended!(Reti),
                0x4E => extended!(Im(0)), // sometimes reported as undefined between Im(0) and Im(1)
                0x4F => extended!(Ld, RegisterImplied(A), RegisterImplied(R)),
                0x50 => extended!(In, PortIndirect(C), RegisterImplied(D)),
                0x51 => extended!(Out, RegisterImplied(D), PortIndirect(C)),
                0x52 => extended!(Sbc, RegisterPairImplied(DE), RegisterPairImplied(HL)),
                0x53 => extended!(Ld, RegisterPairImplied(DE), MemoryDirect(next_doublet(bytes)?)),
                0x54 => extended!(Neg),
                0x55 => extended!(Retn),
                0x56 => extended!(Im(1)),
                0x57 => extended!(Ld, RegisterImplied(I), RegisterImplied(A)),
                0x58 => extended!(In, PortIndirect(C), RegisterImplied(E)),
                0x59 => extended!(Out, RegisterImplied(E), PortIndirect(C)),
                0x5A => extended!(Adc, RegisterPairImplied(DE), RegisterPairImplied(HL)),
                0x5B => extended!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(DE)),
                0x5C => extended!(Neg),
                0x5D => extended!(Retn),
                0x5E => extended!(Im(2)),
                0x5F => extended!(Ld, RegisterImplied(R), RegisterImplied(A)),
                0x60 => extended!(In, PortIndirect(C), RegisterImplied(H)),
                0x61 => extended!(Out, RegisterImplied(H), PortIndirect(C)),
                0x62 => extended!(Sbc, RegisterPairImplied(HL), RegisterPairImplied(HL)),
                0x63 => extended!(Ld, RegisterPairImplied(HL), MemoryDirect(next_doublet(bytes)?)),
                0x64 => extended!(Neg),
                0x65 => extended!(Retn),
                0x66 => extended!(Im(0)),
                0x67 => extended!(Rrd),
                0x68 => extended!(In, PortIndirect(C), RegisterImplied(L)),
                0x69 => extended!(Out, RegisterImplied(L), PortIndirect(C)),
                0x6A => extended!(Adc, RegisterPairImplied(HL), RegisterPairImplied(HL)),
                0x6B => extended!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(HL)),
                0x6C => extended!(Neg),
                0x6D => extended!(Retn),
                0x6E => extended!(Im(0)), // sometimes reported as undefined between Im(0) and Im(1)
                0x6F => extended!(Rld),
                0x70 => extended!(In, source: PortIndirect(C)),
                0x71 => extended!(Out, OctetImmediate(0), PortIndirect(C)),
                0x72 => extended!(Sbc, RegisterPairImplied(SP), RegisterPairImplied(HL)),
                0x73 => extended!(Ld, RegisterPairImplied(SP), MemoryDirect(next_doublet(bytes)?)),
                0x74 => extended!(Neg),
                0x75 => extended!(Retn),
                0x76 => extended!(Im(1)),
                // 0x77
                0x78 => extended!(In, PortIndirect(C), RegisterImplied(A)),
                0x79 => extended!(Out, RegisterImplied(A), PortIndirect(C)),
                0x7A => extended!(Adc, RegisterPairImplied(SP), RegisterPairImplied(HL)),
                0x7B => extended!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(SP)),
                0x7C => extended!(Neg),
                0x7D => extended!(Retn),
                0x7E => extended!(Im(2)),
                // 0x7F ~ 0x9F
                0xA0 => extended!(Ldi),
                0xA1 => extended!(Cpi),
                0xA2 => extended!(Ini),
                0xA3 => extended!(Outi),
                // 0xA4 ~ 0xA7
                0xA8 => extended!(Ldd),
                0xA9 => extended!(Cpd),
                0xAA => extended!(Ind),
                0xAB => extended!(Outd),
                // 0xAC ~ 0xAF
                0xB0 => extended!(Ldir),
                0xB1 => extended!(Cpir),
                0xB2 => extended!(Inir),
                0xB3 => extended!(Otir),
                // 0xB4 ~ 0xB7
                0xB8 => extended!(Lddr),
                0xB9 => extended!(Cpdr),
                0xBA => extended!(Indr),
                0xBB => extended!(Otdr),
                // 0xBC ~ 0xFF
                _ => extended!(Inva),
            }.into()
        }

        /// Decodes a partial bit instruction, whose opcode is known to begin with `0xCB`, `0xDDCB`, or `0xFDCB`.
        /// The prefix, ending with `0xCB`, is assumed to have been read already, so only the remainder is read from `bytes`.
        fn decode_bit_instruction<R: Read>(
            bytes: &mut Bytes<R>,
            index_register: Option<RegisterPairType>,
        ) -> Option<Instruction> {
            // For index bit instructions, the offset is provided before the instruction information.
            let offset =
                if index_register.is_some() { next_byte(bytes)? as i8 } else { Default::default() };
            let opcode = next_byte(bytes)?;

            macro_rules! bitwise {
                ($($args: tt)+) => { instruction!(Opcode { prefix: Some(Bitwise), value: opcode }, $($args)+) }
            }

            macro_rules! indexed_bitwise {
                ($($args: tt)+) => { instruction!(Opcode { prefix: Some(IndexedBitwise(index_register?)), value: opcode }, $($args)+) }
            }

            let instruction = match opcode & 0xF8 {
                0x00 => Rlc,
                0x08 => Rrc,
                0x10 => Rl,
                0x18 => Rr,
                0x20 => Sla,
                0x28 => Sra,
                0x30 => Sll,
                0x38 => Srl,
                0x40 | 0x48 | 0x50 | 0x58 | 0x60 | 0x68 | 0x70 | 0x78 => Bit,
                0x80 | 0x88 | 0x90 | 0x98 | 0xA0 | 0xA8 | 0xB0 | 0xB8 => Res,
                0xC0 | 0xC8 | 0xD0 | 0xD8 | 0xE0 | 0xE8 | 0xF0 | 0xF8 => Set,
                _ => unreachable!("0x{:02X}", opcode),
            };

            let operand_register = match opcode & 0x07 {
                0x00 => Some(B),
                0x01 => Some(C),
                0x02 => Some(D),
                0x03 => Some(E),
                0x04 => Some(H),
                0x05 => Some(L),
                0x06 => None, // HL in non-indexed instructions
                0x07 => Some(A),
                _ => unreachable!(),
            };

            let operand = if opcode < 0x40 {
                match index_register {
                    None => match operand_register {
                        Some(reg) => RegisterImplied(reg),
                        None => MemoryIndirect(HL),
                    },
                    Some(idx) => match operand_register {
                        None => MemoryIndexed(idx, offset),
                        _ => return None,
                    },
                }
            } else {
                let bit = opcode >> 3 & 0x07;
                match index_register {
                    None => match operand_register {
                        Some(reg) => RegisterBitImplied(reg, bit),
                        None => MemoryIndirectBit(HL, bit),
                    },
                    Some(idx) => match operand_register {
                        None => MemoryIndexedBit(idx, offset, bit),
                        _ => return None,
                    },
                }
            };

            if index_register.is_some() {
                Some(indexed_bitwise!(instruction, destination: operand))
            } else {
                Some(bitwise!(instruction, destination: operand))
            }
        }

        /// Decodes a partial index instruction, whose opcode is known to begin with `0xDD` or `0xFD`.
        fn decode_index_instruction<R: Read>(
            bytes: &mut Bytes<R>,
            index_register: RegisterPairType,
        ) -> Option<Instruction> {
            let idx = index_register;
            assert!(idx == IX || idx == IY);

            let opcode = peek_byte(bytes)?;

            macro_rules! indexed {
                ($($args: tt)+) => { instruction!(Opcode { prefix: Some(Indexed(idx)), value: opcode }, $($args)+) }
            }

            match opcode {
                0xDD | 0xFD => return Some(indexed!(Noni)), // when decoding a double prefix, save the second one
                _ => bytes.next(), // otherwise, advance the reader to the next byte
            };

            match opcode {
                // 0x00 ~ 0x08
                0x09 => indexed!(Add, RegisterPairImplied(BC), RegisterPairImplied(idx)),
                // 0x0A ~ 0x18
                0x19 => indexed!(Add, RegisterPairImplied(DE), RegisterPairImplied(idx)),
                // 0x1A ~ 0x20
                0x21 => indexed!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(idx)),
                0x22 => indexed!(Ld, RegisterPairImplied(idx), MemoryDirect(next_doublet(bytes)?)),
                0x23 => indexed!(Inc, destination: RegisterPairImplied(idx)),
                // 0x24 ~ 0x28
                0x29 => indexed!(Add, RegisterPairImplied(idx), RegisterPairImplied(idx)),
                0x2A => indexed!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(idx)),
                0x2B => indexed!(Dec, destination: RegisterPairImplied(idx)),
                // 0x2C ~ 0x33
                0x34 => indexed!(Inc, destination: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x35 => indexed!(Dec, destination: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x36 => indexed!(Ld, destination: MemoryIndexed(idx, next_byte(bytes)? as i8), source: OctetImmediate(next_byte(bytes)?)),
                // 0x37 ~ 0x38
                0x39 => indexed!(Add, RegisterPairImplied(SP), RegisterPairImplied(idx)),
                // 0x3A ~ 0x45
                0x46 => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(B)),
                // 0x47 ~ 0x4D
                0x4E => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(C)),
                // 0x4F ~ 0x55
                0x56 => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(D)),
                // 0x57 ~ 0x5D
                0x5E => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(E)),
                // 0x5F ~ 0x65
                0x66 => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(H)),
                // 0x67 ~ 0x6D
                0x6E => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(L)),
                // 0x6F
                0x70 => indexed!(Ld, RegisterImplied(B), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x71 => indexed!(Ld, RegisterImplied(C), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x72 => indexed!(Ld, RegisterImplied(D), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x73 => indexed!(Ld, RegisterImplied(E), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x74 => indexed!(Ld, RegisterImplied(H), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                0x75 => indexed!(Ld, RegisterImplied(L), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0x76
                0x77 => indexed!(Ld, RegisterImplied(A), MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0x78 ~ 0x7D
                0x7E => indexed!(Ld, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(A)),
                // 0x7F ~ 0x85
                0x86 => indexed!(Add, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(A)),
                // 0x87 ~ 0x8D
                0x8E => indexed!(Adc, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(A)),
                // 0x8F ~ 0x95
                0x96 => indexed!(Sub, source: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0x97 ~ 0x9D
                0x9E => indexed!(Sbc, MemoryIndexed(idx, next_byte(bytes)? as i8), RegisterImplied(A)),
                // 0x9F ~ 0xA5
                0xA6 => indexed!(And, source: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0xA7 ~ 0xAD
                0xAE => indexed!(Xor, source: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0xAF ~ 0xB5
                0xB6 => indexed!(Or, source: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0xB7 ~ 0xBD
                0xBE => indexed!(Cp, source: MemoryIndexed(idx, next_byte(bytes)? as i8)),
                // 0xBF ~ 0xCA
                0xCB => decode_bit_instruction(bytes, Some(idx))?,
                // 0xCC ~ 0xE0
                0xE1 => indexed!(Pop, destination: RegisterPairImplied(idx)),
                // 0xE2
                0xE3 => indexed!(Ex, RegisterPairImplied(idx), MemoryIndirect(SP)),
                // 0xE4
                0xE5 => indexed!(Push, source: RegisterPairImplied(idx)),
                // 0xE6 ~ 0xE8
                0xE9 => indexed!(Jp(None), source: MemoryIndirect(idx)),
                // 0xEA ~ 0xF8
                0xF9 => indexed!(Ld, RegisterPairImplied(idx), RegisterPairImplied(SP)),
                // 0xFA ~ 0xFF
                _ => return None,
            }.into()
        }

        // Parse the instruction opcode and operands byte by byte.
        let opcode = next_byte(bytes)?;

        macro_rules! root {
            ($($args: tt)+) => { instruction!(Opcode { prefix: None, value: opcode }, $($args)+) }
        }

        match opcode {
            0x00 => root!(Nop),
            0x01 => root!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(BC)),
            0x02 => root!(Ld, RegisterImplied(A), MemoryIndirect(BC)),
            0x03 => root!(Inc, destination: RegisterPairImplied(BC)),
            0x04 => root!(Inc, destination: RegisterImplied(B)),
            0x05 => root!(Dec, destination: RegisterImplied(B)),
            0x06 => root!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(B)),
            0x07 => root!(Rlca),
            0x08 => root!(Ex, RegisterPairImplied(AF_), RegisterPairImplied(AF)),
            0x09 => root!(Add, RegisterPairImplied(BC), RegisterPairImplied(HL)),
            0x0A => root!(Ld, MemoryIndirect(BC), RegisterImplied(A)),
            0x0B => root!(Dec, destination: RegisterPairImplied(BC)),
            0x0C => root!(Inc, destination: RegisterImplied(C)),
            0x0D => root!(Dec, destination: RegisterImplied(C)),
            0x0E => root!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(C)),
            0x0F => root!(Rrca),
            0x10 => root!(Djnz, source: OctetImmediate(next_byte(bytes)?)),
            0x11 => root!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(DE)),
            0x12 => root!(Ld, RegisterImplied(A), MemoryIndirect(DE)),
            0x13 => root!(Inc, destination: RegisterPairImplied(DE)),
            0x14 => root!(Inc, destination: RegisterImplied(D)),
            0x15 => root!(Dec, destination: RegisterImplied(D)),
            0x16 => root!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(D)),
            0x17 => root!(Rla),
            0x18 => root!(Jr(None), source: OctetImmediate(next_byte(bytes)?)),
            0x19 => root!(Add, RegisterPairImplied(DE), RegisterPairImplied(HL)),
            0x1A => root!(Ld, MemoryIndirect(DE), RegisterImplied(A)),
            0x1B => root!(Dec, destination: RegisterPairImplied(DE)),
            0x1C => root!(Inc, destination: RegisterImplied(E)),
            0x1D => root!(Dec, destination: RegisterImplied(E)),
            0x1E => root!(Ld, RegisterImplied(E), OctetImmediate(next_byte(bytes)?)),
            0x1F => root!(Rra),
            0x20 => root!(Jr(Some(FlagNotSet(Flag::Z))), source: OctetImmediate(next_byte(bytes)?)),
            0x21 => root!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(HL)),
            0x22 => root!(Ld, RegisterPairImplied(HL), MemoryDirect(next_doublet(bytes)?)),
            0x23 => root!(Inc, destination: RegisterPairImplied(HL)),
            0x24 => root!(Inc, destination: RegisterImplied(H)),
            0x25 => root!(Dec, destination: RegisterImplied(H)),
            0x26 => root!(Ld, OctetImmediate(next_byte(bytes)?), RegisterImplied(H)),
            0x27 => root!(Daa),
            0x28 => root!(Jr(Some(FlagSet(Flag::Z))), source: OctetImmediate(next_byte(bytes)?)),
            0x29 => root!(Add, RegisterPairImplied(HL), RegisterPairImplied(HL)),
            0x2A => root!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterPairImplied(HL)),
            0x2B => root!(Dec, destination: RegisterPairImplied(HL)),
            0x2C => root!(Inc, destination: RegisterImplied(L)),
            0x2D => root!(Dec, destination: RegisterImplied(L)),
            0x2E => root!(Ld, RegisterImplied(L), OctetImmediate(next_byte(bytes)?)),
            0x2F => root!(Cpl),
            0x30 => root!(Jr(Some(FlagNotSet(Flag::C))), source: OctetImmediate(next_byte(bytes)?)),
            0x31 => root!(Ld, DoubletImmediate(next_doublet(bytes)?), RegisterPairImplied(SP)),
            0x32 => root!(Ld, RegisterImplied(A), MemoryDirect(next_doublet(bytes)?)),
            0x33 => root!(Inc, destination: RegisterPairImplied(SP)),
            0x34 => root!(Inc, destination: MemoryIndirect(HL)),
            0x35 => root!(Dec, destination: MemoryIndirect(HL)),
            0x36 => root!(Ld, OctetImmediate(next_byte(bytes)?), MemoryIndirect(HL)),
            0x37 => root!(Scf),
            0x38 => root!(Jr(Some(FlagSet(Flag::C))), source: OctetImmediate(next_byte(bytes)?)),
            0x39 => root!(Add, RegisterPairImplied(SP), RegisterPairImplied(HL)),
            0x3A => root!(Ld, MemoryDirect(next_doublet(bytes)?), RegisterImplied(A)),
            0x3B => root!(Dec, destination: RegisterPairImplied(SP)),
            0x3C => root!(Inc, destination: RegisterImplied(A)),
            0x3D => root!(Dec, destination: RegisterImplied(A)),
            0x3E => root!(Ld, RegisterImplied(A), OctetImmediate(next_byte(bytes)?)),
            0x3F => root!(Ccf),
            0x40 => root!(Ld, RegisterImplied(B), RegisterImplied(B)),
            0x41 => root!(Ld, RegisterImplied(C), RegisterImplied(B)),
            0x42 => root!(Ld, RegisterImplied(D), RegisterImplied(B)),
            0x43 => root!(Ld, RegisterImplied(E), RegisterImplied(B)),
            0x44 => root!(Ld, RegisterImplied(H), RegisterImplied(B)),
            0x45 => root!(Ld, RegisterImplied(L), RegisterImplied(B)),
            0x46 => root!(Ld, MemoryIndirect(HL), RegisterImplied(B)),
            0x47 => root!(Ld, RegisterImplied(A), RegisterImplied(C)),
            0x48 => root!(Ld, RegisterImplied(B), RegisterImplied(C)),
            0x49 => root!(Ld, RegisterImplied(C), RegisterImplied(C)),
            0x4A => root!(Ld, RegisterImplied(D), RegisterImplied(C)),
            0x4B => root!(Ld, RegisterImplied(E), RegisterImplied(C)),
            0x4C => root!(Ld, RegisterImplied(H), RegisterImplied(C)),
            0x4D => root!(Ld, RegisterImplied(L), RegisterImplied(C)),
            0x4E => root!(Ld, MemoryIndirect(HL), RegisterImplied(C)),
            0x4F => root!(Ld, RegisterImplied(A), RegisterImplied(C)),
            0x50 => root!(Ld, RegisterImplied(B), RegisterImplied(D)),
            0x51 => root!(Ld, RegisterImplied(C), RegisterImplied(D)),
            0x52 => root!(Ld, RegisterImplied(D), RegisterImplied(D)),
            0x53 => root!(Ld, RegisterImplied(E), RegisterImplied(D)),
            0x54 => root!(Ld, RegisterImplied(H), RegisterImplied(D)),
            0x55 => root!(Ld, RegisterImplied(L), RegisterImplied(D)),
            0x56 => root!(Ld, MemoryIndirect(HL), RegisterImplied(D)),
            0x57 => root!(Ld, RegisterImplied(A), RegisterImplied(D)),
            0x58 => root!(Ld, RegisterImplied(B), RegisterImplied(E)),
            0x59 => root!(Ld, RegisterImplied(C), RegisterImplied(E)),
            0x5A => root!(Ld, RegisterImplied(D), RegisterImplied(E)),
            0x5B => root!(Ld, RegisterImplied(E), RegisterImplied(E)),
            0x5C => root!(Ld, RegisterImplied(H), RegisterImplied(E)),
            0x5D => root!(Ld, RegisterImplied(L), RegisterImplied(E)),
            0x5E => root!(Ld, MemoryIndirect(HL), RegisterImplied(E)),
            0x5F => root!(Ld, RegisterImplied(A), RegisterImplied(E)),
            0x60 => root!(Ld, RegisterImplied(B), RegisterImplied(H)),
            0x61 => root!(Ld, RegisterImplied(C), RegisterImplied(H)),
            0x62 => root!(Ld, RegisterImplied(D), RegisterImplied(H)),
            0x63 => root!(Ld, RegisterImplied(E), RegisterImplied(H)),
            0x64 => root!(Ld, RegisterImplied(H), RegisterImplied(H)),
            0x65 => root!(Ld, RegisterImplied(L), RegisterImplied(H)),
            0x66 => root!(Ld, MemoryIndirect(HL), RegisterImplied(H)),
            0x67 => root!(Ld, RegisterImplied(A), RegisterImplied(H)),
            0x68 => root!(Ld, RegisterImplied(B), RegisterImplied(L)),
            0x69 => root!(Ld, RegisterImplied(C), RegisterImplied(L)),
            0x6A => root!(Ld, RegisterImplied(D), RegisterImplied(L)),
            0x6B => root!(Ld, RegisterImplied(E), RegisterImplied(L)),
            0x6C => root!(Ld, RegisterImplied(H), RegisterImplied(L)),
            0x6D => root!(Ld, RegisterImplied(L), RegisterImplied(L)),
            0x6E => root!(Ld, MemoryIndirect(HL), RegisterImplied(L)),
            0x6F => root!(Ld, RegisterImplied(A), RegisterImplied(L)),
            0x70 => root!(Ld, RegisterImplied(B), MemoryIndirect(HL)),
            0x71 => root!(Ld, RegisterImplied(C), MemoryIndirect(HL)),
            0x72 => root!(Ld, RegisterImplied(D), MemoryIndirect(HL)),
            0x73 => root!(Ld, RegisterImplied(E), MemoryIndirect(HL)),
            0x74 => root!(Ld, RegisterImplied(H), MemoryIndirect(HL)),
            0x75 => root!(Ld, RegisterImplied(L), MemoryIndirect(HL)),
            0x76 => root!(Halt),
            0x77 => root!(Ld, RegisterImplied(A), MemoryIndirect(HL)),
            0x78 => root!(Ld, RegisterImplied(B), RegisterImplied(A)),
            0x79 => root!(Ld, RegisterImplied(C), RegisterImplied(A)),
            0x7A => root!(Ld, RegisterImplied(D), RegisterImplied(A)),
            0x7B => root!(Ld, RegisterImplied(E), RegisterImplied(A)),
            0x7C => root!(Ld, RegisterImplied(H), RegisterImplied(A)),
            0x7D => root!(Ld, RegisterImplied(L), RegisterImplied(A)),
            0x7E => root!(Ld, MemoryIndirect(HL), RegisterImplied(A)),
            0x7F => root!(Ld, RegisterImplied(A), RegisterImplied(A)),
            0x80 => root!(Add, RegisterImplied(B), RegisterImplied(A)),
            0x81 => root!(Add, RegisterImplied(C), RegisterImplied(A)),
            0x82 => root!(Add, RegisterImplied(D), RegisterImplied(A)),
            0x83 => root!(Add, RegisterImplied(E), RegisterImplied(A)),
            0x84 => root!(Add, RegisterImplied(H), RegisterImplied(A)),
            0x85 => root!(Add, RegisterImplied(L), RegisterImplied(A)),
            0x86 => root!(Add, MemoryIndirect(HL), RegisterImplied(A)),
            0x87 => root!(Add, RegisterImplied(A), RegisterImplied(A)),
            0x88 => root!(Adc, RegisterImplied(B), RegisterImplied(A)),
            0x89 => root!(Adc, RegisterImplied(C), RegisterImplied(A)),
            0x8A => root!(Adc, RegisterImplied(D), RegisterImplied(A)),
            0x8B => root!(Adc, RegisterImplied(E), RegisterImplied(A)),
            0x8C => root!(Adc, RegisterImplied(H), RegisterImplied(A)),
            0x8D => root!(Adc, RegisterImplied(L), RegisterImplied(A)),
            0x8E => root!(Adc, MemoryIndirect(HL), RegisterImplied(A)),
            0x8F => root!(Adc, RegisterImplied(A), RegisterImplied(A)),
            0x90 => root!(Sub, source: RegisterImplied(B)),
            0x91 => root!(Sub, source: RegisterImplied(C)),
            0x92 => root!(Sub, source: RegisterImplied(D)),
            0x93 => root!(Sub, source: RegisterImplied(E)),
            0x94 => root!(Sub, source: RegisterImplied(H)),
            0x95 => root!(Sub, source: RegisterImplied(L)),
            0x96 => root!(Sub, source: MemoryIndirect(HL)),
            0x97 => root!(Sub, source: RegisterImplied(A)),
            0x98 => root!(Sbc, RegisterImplied(B), RegisterImplied(A)),
            0x99 => root!(Sbc, RegisterImplied(C), RegisterImplied(A)),
            0x9A => root!(Sbc, RegisterImplied(D), RegisterImplied(A)),
            0x9B => root!(Sbc, RegisterImplied(E), RegisterImplied(A)),
            0x9C => root!(Sbc, RegisterImplied(H), RegisterImplied(A)),
            0x9D => root!(Sbc, RegisterImplied(L), RegisterImplied(A)),
            0x9E => root!(Sbc, MemoryIndirect(HL), RegisterImplied(A)),
            0x9F => root!(Sbc, RegisterImplied(A), RegisterImplied(A)),
            0xA0 => root!(And, source: RegisterImplied(B)),
            0xA1 => root!(And, source: RegisterImplied(C)),
            0xA2 => root!(And, source: RegisterImplied(D)),
            0xA3 => root!(And, source: RegisterImplied(E)),
            0xA4 => root!(And, source: RegisterImplied(H)),
            0xA5 => root!(And, source: RegisterImplied(L)),
            0xA6 => root!(And, source: MemoryIndirect(HL)),
            0xA7 => root!(And, source: RegisterImplied(A)),
            0xA8 => root!(Xor, source: RegisterImplied(B)),
            0xA9 => root!(Xor, source: RegisterImplied(C)),
            0xAA => root!(Xor, source: RegisterImplied(D)),
            0xAB => root!(Xor, source: RegisterImplied(E)),
            0xAC => root!(Xor, source: RegisterImplied(H)),
            0xAD => root!(Xor, source: RegisterImplied(L)),
            0xAE => root!(Xor, source: MemoryIndirect(HL)),
            0xAF => root!(Xor, source: RegisterImplied(A)),
            0xB0 => root!(Or, source: RegisterImplied(B)),
            0xB1 => root!(Or, source: RegisterImplied(C)),
            0xB2 => root!(Or, source: RegisterImplied(D)),
            0xB3 => root!(Or, source: RegisterImplied(E)),
            0xB4 => root!(Or, source: RegisterImplied(H)),
            0xB5 => root!(Or, source: RegisterImplied(L)),
            0xB6 => root!(Or, source: MemoryIndirect(HL)),
            0xB7 => root!(Or, source: RegisterImplied(A)),
            0xB8 => root!(Cp, source: RegisterImplied(B)),
            0xB9 => root!(Cp, source: RegisterImplied(C)),
            0xBA => root!(Cp, source: RegisterImplied(D)),
            0xBB => root!(Cp, source: RegisterImplied(E)),
            0xBC => root!(Cp, source: RegisterImplied(H)),
            0xBD => root!(Cp, source: RegisterImplied(L)),
            0xBE => root!(Cp, source: MemoryIndirect(HL)),
            0xBF => root!(Cp, source: RegisterImplied(A)),
            0xC0 => root!(Ret(Some(FlagNotSet(Flag::Z)))),
            0xC1 => root!(Pop, destination: RegisterPairImplied(BC)),
            0xC2 => root!(Jp(Some(FlagNotSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xC3 => root!(Jp(None), source: DoubletImmediate(next_doublet(bytes)?)),
            0xC4 => root!(Call(Some(FlagNotSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xC5 => root!(Push, source: RegisterPairImplied(BC)),
            0xC6 => root!(Add, OctetImmediate(next_byte(bytes)?), RegisterImplied(A)),
            0xC7 => root!(Rst(0x00)),
            0xC8 => root!(Ret(Some(FlagSet(Flag::Z)))),
            0xC9 => root!(Ret(None)),
            0xCA => root!(Jp(Some(FlagSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xCB => decode_bit_instruction(bytes, None)?,
            0xCC => root!(Call(Some(FlagSet(Flag::Z))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xCD => root!(Call(None), source: DoubletImmediate(next_doublet(bytes)?)),
            0xCE => root!(Adc, OctetImmediate(next_byte(bytes)?), RegisterImplied(A)),
            0xCF => root!(Rst(0x08)),
            0xD0 => root!(Ret(Some(FlagNotSet(Flag::C)))),
            0xD1 => root!(Pop, destination: RegisterPairImplied(DE)),
            0xD2 => root!(Jp(Some(FlagNotSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xD3 => root!(Out, RegisterImplied(A), PortDirect(next_byte(bytes)?)),
            0xD4 => root!(Call(Some(FlagNotSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xD5 => root!(Push, source: RegisterPairImplied(DE)),
            0xD6 => root!(Sub, source: OctetImmediate(next_byte(bytes)?)),
            0xD7 => root!(Rst(0x10)),
            0xD8 => root!(Ret(Some(FlagSet(Flag::C)))),
            0xD9 => root!(Exx),
            0xDA => root!(Jp(Some(FlagSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xDB => root!(Out, PortDirect(next_byte(bytes)?), RegisterImplied(A)),
            0xDC => root!(Call(Some(FlagSet(Flag::C))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xDD => decode_index_instruction(bytes, IX)?,
            0xDE => root!(Sbc, OctetImmediate(next_byte(bytes)?), RegisterImplied(A)),
            0xDF => root!(Rst(0x18)),
            0xE0 => root!(Ret(Some(FlagSet(Flag::PV)))),
            0xE1 => root!(Pop, destination: RegisterPairImplied(HL)),
            0xE2 => root!(Jp(Some(FlagSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xE3 => root!(Ex, RegisterPairImplied(HL), MemoryIndirect(SP)),
            0xE4 => root!(Call(Some(FlagSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xE5 => root!(Push, source: RegisterPairImplied(HL)),
            0xE6 => root!(And, source: OctetImmediate(next_byte(bytes)?)),
            0xE7 => root!(Rst(0x20)),
            0xE8 => root!(Ret(Some(FlagNotSet(Flag::PV)))),
            0xE9 => root!(Jp(None), source: MemoryIndirect(HL)),
            0xEA => root!(Jp(Some(FlagNotSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xEB => root!(Ex, RegisterPairImplied(HL), RegisterPairImplied(DE)),
            0xEC => root!(Call(Some(FlagNotSet(Flag::PV))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xED => decode_extended_instruction(bytes)?,
            0xEE => root!(Xor, source: OctetImmediate(next_byte(bytes)?)),
            0xEF => root!(Rst(0x28)),
            0xF0 => root!(Ret(Some(FlagSet(Flag::S)))),
            0xF1 => root!(Pop, destination: RegisterPairImplied(AF)),
            0xF2 => root!(Jp(Some(FlagSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xF3 => root!(Di),
            0xF4 => root!(Call(Some(FlagSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xF5 => root!(Push, source: RegisterPairImplied(AF)),
            0xF6 => root!(Or, source: OctetImmediate(next_byte(bytes)?)),
            0xF7 => root!(Rst(0x30)),
            0xF8 => root!(Ret(Some(FlagNotSet(Flag::S)))),
            0xF9 => root!(Ld, RegisterPairImplied(HL), RegisterPairImplied(SP)),
            0xFA => root!(Jp(Some(FlagNotSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xFB => root!(Ei),
            0xFC => root!(Call(Some(FlagNotSet(Flag::S))), source: DoubletImmediate(next_doublet(bytes)?)),
            0xFD => decode_index_instruction(bytes, IY)?,
            0xFE => root!(Cp, source: OctetImmediate(next_byte(bytes)?)),
            0xFF => root!(Rst(0x38)),
        }.into()
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

        match self.r#type {
            Noni | Inva => write!(f, "{}", self.opcode),
            _ => match (self.source, self.destination) {
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
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_opcode_prefix_doublet() {
        let indexed_bit_ix = OpcodePrefix::IndexedBitwise(RegisterPairType::IX);
        assert_eq!("DD CB", format!("{}", indexed_bit_ix));
    }

    #[test]
    fn decode_instruction() {
        let inc_b = Instruction::decode(&mut [0x04].bytes()).unwrap();
        assert_eq!(None, inc_b.source);
        assert_eq!(Some(Operand::RegisterImplied(SingleRegisterType::B)), inc_b.destination);
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
    fn decode_instruction_cb() {
        for opcode in 0x00..=0xFF {
            let cb_instruction = Instruction::decode(&mut [0xCB, opcode].bytes());
            assert!(cb_instruction.is_some());
        }
    }

    #[test]
    fn decode_instruction_bit() {
        for opcode in 0x40..=0xFF {
            let bit_instruction = Instruction::decode(&mut [0xCB, opcode].bytes()).unwrap();
            let offset = 0x40 * (opcode / 0x40);

            let expected_instruction = match offset {
                0x40 => InstructionType::Bit,
                0x80 => InstructionType::Res,
                0xC0 => InstructionType::Set,
                _ => unreachable!(),
            };
            let actual_instruction = bit_instruction.r#type;

            let expected_bit = (opcode - offset) / 8;
            let actual_bit = match bit_instruction.destination.unwrap() {
                Operand::RegisterBitImplied(_, val) => val,
                Operand::MemoryIndirectBit(_, val) => val,
                _ => panic!("Unexpected decoded operand"),
            };

            assert_eq!(expected_instruction, actual_instruction);
            assert_eq!(expected_bit, actual_bit);
        }
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

    #[test]
    fn format_invalid_instruction() {
        let invalid = Instruction::decode(&mut [0xED, 0x04].bytes()).unwrap();
        assert_eq!("ED 04", format!("{}", invalid));
    }
}
