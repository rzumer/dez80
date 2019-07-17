#[macro_use]
pub mod operation;
mod common;
pub mod instruction;
pub mod register;

pub use common::*;
pub use instruction::Instruction;
pub use operation::Operation;
pub use register::*;
