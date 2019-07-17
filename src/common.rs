use crate::storage::*;
use std::fmt;

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
            MemoryRelative(val) => write!(f, "0x{:02x}", val),
            MemoryIndirect(reg) => write!(f, "({})", reg),
            MemoryIndexed(reg, idx) => write!(f, "({} + 0x{:02x})", reg, idx),
            MemoryZeroPage(val) => write!(f, "0x{:02x}", val),
            MemoryBitIndirect(reg, bit) => write!(f, "{}, ({})", bit, reg),
            MemoryBitIndexed(reg, idx, bit) => write!(f, "{}, ({} + 0x{:02x})", bit, reg, idx),
            PortDirect(val) => write!(f, "(0x{:02x})", val),
            PortIndirect(reg) => write!(f, "({})", reg),
        }
    }
}
