use super::storage::*;

macro_rules! op {
    ($type:ident, $length: expr, $source:ident, $dest:ident) => {
        Operation {
            r#type: OperationType::$type,
            cycles: $length,
            source: $source,
            destination: $dest
        }
    };
    ($type:ident, $length: expr) => {
        Operation {
            r#type: OperationType::$type,
            cycles: $length,
            source: None,
            destination: None
        }
    };
}

pub enum OperationType {
    None
}

pub enum DataLocation {
    Register(RegisterType),
    RegisterPair(RegisterPairType),
    Memory(u16)
}

/// Represents a unit of work with machine cycle granularity for a Z80 CPU.
pub struct Operation {
    pub r#type: OperationType,
    pub cycles: usize,
    pub source: Option<DataLocation>,
    pub destination: Option<DataLocation>
}

pub const NO_OP: Operation = op!(None, 4);
