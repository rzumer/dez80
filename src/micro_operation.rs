use crate::common::{Condition, Operand};

macro_rules! micro_op {
    ($type: expr, $cycles: expr, $source: expr, $destination: expr, $condition: expr) => {
        MicroOperation {
            r#type: $type,
            cycles: $cycles,
            source: Some($source),
            destination: Some($destination),
            condition: Some($condition),
        }
    };
    ($type: expr, $cycles: expr, $source: expr, $destination: expr) => {
        MicroOperation {
            r#type: $type,
            cycles: $cycles,
            source: $source,
            destination: $destination,
            condition: None,
        }
    };
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

/// Defines a micro-operation matching the `NOP` instruction.
pub const NO_OP: MicroOperation = micro_op!(MicroOperationType::NoOperation, 4);

/// Represents a type of micro-operation that informs the
/// CPU's execution strategy.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MicroOperationType {
    Add,
    Decrement,
    Exchange,
    Increment,
    Load,
    NoOperation,
    RotateLeftThroughCarry,
    RotateRightThroughCarry,
    Subtract,
}

/// Represents a unit of operation no larger in scope than an instruction
/// or machine cycle, and which can be smaller.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MicroOperation {
    pub r#type: MicroOperationType,
    pub cycles: usize,
    pub source: Option<Operand>,
    pub destination: Option<Operand>,
    pub condition: Option<Condition>,
}

impl Default for MicroOperation {
    fn default() -> Self {
        NO_OP
    }
}
