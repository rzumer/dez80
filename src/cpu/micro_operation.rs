use super::storage::{Flag, RegisterPairType, RegisterSet, RegisterType};

macro_rules! micro_op {
    ($type: expr, $cycles: expr) => {
        MicroOperation {
            r#type: $type,
            cycles: $cycles,
            source: None,
            destination: None,
            condition: None,
        }
    };
}

/// Represents a unit of operation no larger in scope than an instruction
/// or machine cycle, and which can be smaller.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MicroOperation {
    pub r#type: MicroOperationType,
    pub cycles: usize,
    pub source: Option<DataLocation>,
    pub destination: Option<DataLocation>,
    pub condition: Option<Condition>,
}

/// Represents a type of micro-operation that informs the
/// CPU's execution strategy.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MicroOperationType {
    None,
    Load,
}

/// Represents a target for data operations.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DataLocation {
    Register(RegisterType),
    RegisterPair(RegisterPairType),
    RegisterIndex(RegisterPairType, u8),
    MemoryImmediate(u16),
    MemoryRelative(u8),
    MemoryIndirect(RegisterPairType),
    Port(u8),
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

/// Defines a micro-operation matching the `NOP` instruction.
pub const NO_OP: MicroOperation = micro_op!(MicroOperationType::None, 4);
