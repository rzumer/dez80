//! Provides Z80 disassembly and analysis models and functions.

extern crate strum;
extern crate strum_macros;

pub mod decoder;
pub mod instruction;
pub mod register;

pub use decoder::InstructionDecoder;
pub use instruction::Instruction;
pub use register::*;
