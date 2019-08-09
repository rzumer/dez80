//! Contains models and functions that are shared between other modules.

use crate::register::*;
use std::fmt;

/// Represents a condition that must be true for an operation to run.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Condition {
    FlagSet(Flag),
    FlagNotSet(Flag),
    RegisterZero(SingleRegisterType),
    RegisterPairZero(RegisterPairType),
}

/// Represents a target for data operations.
/// Variants are closely related to Z80 addressing modes.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operand {
    OctetImmediate(u8),
    DoubletImmediate(u16),
    RegisterImplied(SingleRegisterType),
    RegisterPairImplied(RegisterPairType),
    RegisterBitImplied(SingleRegisterType, u8),
    MemoryDirect(u16),
    MemoryRelative(i8),
    MemoryIndirect(RegisterPairType),
    MemoryIndexed(RegisterPairType, i8),
    MemoryIndexedWithRegisterCopy(RegisterPairType, i8, SingleRegisterType),
    MemoryIndirectBit(RegisterPairType, u8),
    MemoryIndexedBit(RegisterPairType, i8, u8),
    MemoryIndexedBitWithRegisterCopy(RegisterPairType, i8, u8, SingleRegisterType),
    PortDirect(u8),
    PortIndirect(SingleRegisterType),
}

impl fmt::Display for Operand {
    /// Formats operands based on standard notation.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Operand::*;

        match self {
            OctetImmediate(val) => write!(f, "0x{:02x}", val),
            DoubletImmediate(val) => write!(f, "0x{:04x}", val),
            RegisterImplied(reg) => write!(f, "{}", reg),
            RegisterPairImplied(reg) => write!(f, "{}", reg),
            RegisterBitImplied(reg, bit) => write!(f, "{}, {}", bit, reg),
            MemoryDirect(val) => write!(f, "(0x{:04x})", val.to_le()),
            MemoryRelative(val) => write!(f, "0x{:02x}", *val as u8),
            MemoryIndirect(reg) => write!(f, "({})", reg),
            MemoryIndexed(reg, idx) => write!(f, "({} + 0x{:02x})", reg, *idx as u8),
            MemoryIndexedWithRegisterCopy(reg_in, idx, reg_out) => {
                write!(f, "({} + 0x{:02x}), {}", reg_in, *idx as u8, reg_out)
            }
            MemoryIndirectBit(reg, bit) => write!(f, "{}, ({})", bit, reg),
            MemoryIndexedBit(reg, idx, bit) => {
                write!(f, "{}, ({} + 0x{:02x})", bit, reg, *idx as u8)
            }
            MemoryIndexedBitWithRegisterCopy(reg_in, idx, bit, reg_out) => {
                write!(f, "{}, ({} + 0x{:02x}), {}", bit, reg_in, *idx as u8, reg_out)
            }
            PortDirect(val) => write!(f, "(0x{:02x})", val),
            PortIndirect(reg) => write!(f, "({})", reg),
        }
    }
}
