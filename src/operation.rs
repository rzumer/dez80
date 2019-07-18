use crate::common::{Condition, Operand};

macro_rules! operation {
    ($type: expr, $cycles: expr, $source: expr, $destination: expr, $condition: expr) => {
        Operation {
            r#type: $type,
            cycles: $cycles,
            source: Some($source),
            destination: Some($destination),
            condition: Some($condition),
        }
    };
    ($type: expr, $cycles: expr, $source: expr, $destination: expr) => {
        Operation {
            r#type: $type,
            cycles: $cycles,
            source: $source,
            destination: $destination,
            condition: None,
        }
    };
    ($type: expr, $cycles: expr) => {
        Operation {
            r#type: $type,
            cycles: $cycles,
            source: None,
            destination: None,
            condition: None,
        }
    };
}

/// Defines an operation matching the `NOP` instruction.
pub const NO_OP: Operation = operation!(OperationType::NoOperation, 4);

/// Represents a type of operation that informs the CPU's execution strategy.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperationType {
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
pub struct Operation {
    pub r#type: OperationType,
    pub cycles: usize,
    pub source: Option<Operand>,
    pub destination: Option<Operand>,
    pub condition: Option<Condition>,
}

impl Default for Operation {
    fn default() -> Self {
        NO_OP
    }
}
