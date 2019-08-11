//! Contains models and functions used to manage a Z80 processor's internal storage,
//! including single registers, register pairs, and status flags.

use std::fmt;
use strum_macros::Display;

macro_rules! impl_display_register {
    ($( $ident: ident ),*) => (
        $( impl fmt::Display for $ident {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                // Write out the enum variant name, replacing underscores with straight quotes.
                write!(f, "{}", format!("{:?}", self).replace("_", "'"))
            }
        } )*
    );
}

/// Used to identify a single register in a manner
/// independent of its representation in a `RegisterSet`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SingleRegisterType {
    A,   // Accumulator register
    F,   // Flag register
    B,   // B general purpose register
    C,   // C general purpose register
    D,   // D general purpose register
    E,   // E general purpose register
    H,   // H general purpose register
    L,   // L general purpose register
    IXH, // IX index register, high byte
    IXL, // IX index register, low byte
    IYH, // IY index register, high byte
    IYL, // IY index register, low byte
    PCH, // Program counter, high byte
    PCL, // Program counter, low byte
    SPH, // Stack pointer, high byte
    SPL, // Stack pointer, low byte
    I,   // Interrupt vector register
    R,   // Memory refresh register
    W,   // Temporary storage register, high byte
    Z,   // Temporary storage register, low byte
    A_,  // Alternate accumulator register
    F_,  // Alternate flag register
    B_,  // Alternate B general purpose register
    C_,  // Alternate C general purpose register
    D_,  // Alternate D general purpose register
    E_,  // Alternate E general purpose register
    H_,  // Alternate H general purpose register
    L_,  // Alternate L general purpose register
}

/// Used to identify a register pair in a manner
/// independent of its representation in a `RegisterSet`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegisterPairType {
    AF,  // Accumulator and flag registers
    BC,  // B and C general purpose registers
    DE,  // D and E general purpose registers
    HL,  // H and L general purpose registers
    IX,  // IX index register
    IY,  // IY index register
    PC,  // Program counter
    SP,  // Stack pointer
    IR,  // Interrupt vector and memory refresh registers
    WZ,  // Temporary storage register
    AF_, // Alternate accumulator and flag registers
    BC_, // Alternate B and C general purpose registers
    DE_, // Alternate D and E general purpose registers
    HL_, // Alternate H and L general purpose registers
}

impl RegisterPairType {
    pub fn to_single_register_types(self) -> (SingleRegisterType, SingleRegisterType) {
        use RegisterPairType::*;
        use SingleRegisterType::*;

        match self {
            AF => (A, F),
            BC => (B, C),
            DE => (D, E),
            HL => (H, L),
            IX => (IXH, IXL),
            IY => (IYH, IYL),
            PC => (PCH, PCL),
            SP => (SPH, SPL),
            IR => (I, R),
            WZ => (W, Z),
            AF_ => (A_, F_),
            BC_ => (B_, C_),
            DE_ => (D_, E_),
            HL_ => (H_, L_),
        }
    }
}

impl_display_register!(SingleRegisterType, RegisterPairType);

/// Used to identify either a single register or a register pair.
pub enum RegisterType {
    SingleRegister(SingleRegisterType),
    RegisterPair(RegisterPairType),
}

/// Represents a single-byte register.
pub type Register = u8;

/// Represents a pair of single-byte registers.
#[derive(Clone, Copy, Default)]
pub struct RegisterPair {
    pub high: Register,
    pub low: Register,
}

impl RegisterPair {
    pub fn new(value: u16) -> Self {
        let mut output = RegisterPair::default();
        output.set_full(value);

        output
    }

    pub fn full(self) -> u16 {
        u16::from_be_bytes([self.high, self.low])
    }
    pub fn set_full(&mut self, value: u16) {
        let bytes = value.to_be_bytes();
        self.high = bytes[0];
        self.low = bytes[1];
    }
}

/// Represents individual Z80 status flags.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Display, PartialEq)]
pub enum Flag {
    S = 0b1000_0000,
    Z = 0b0100_0000,
    F5 = 0b0010_0000,
    H = 0b0001_0000,
    F3 = 0b0000_1000,
    PV = 0b0000_0100,
    N = 0b0000_0010,
    C = 0b0000_0001,
}

impl Flag {
    pub const SIGN: Self = Flag::S;
    pub const ZERO: Self = Flag::Z;
    pub const BIT5: Self = Flag::F5;
    pub const HALF_CARRY: Self = Flag::H;
    pub const BIT3: Self = Flag::F3;
    pub const PARITY: Self = Flag::PV;
    pub const OVERFLOW: Self = Flag::PV;
    pub const SUBTRACT: Self = Flag::N;
    pub const CARRY: Self = Flag::C;
}

/// Represents a set of Z80 status flags as stored in a register.
/// The backing data is a `Register` stored as a reference.
pub struct FlagSet<'a> {
    pub full: &'a Register,
}

impl<'a> FlagSet<'a> {
    pub fn flag(&self, flag: Flag) -> bool {
        *self.full & (flag as u8) > 0
    }
}

impl<'a> From<&'a u8> for FlagSet<'a> {
    fn from(value: &'a u8) -> Self {
        FlagSet { full: value }
    }
}

/// A mutable variant of `FlagSet`.
pub struct FlagSetMut<'a> {
    pub full: &'a mut Register,
}

impl<'a> FlagSetMut<'a> {
    pub fn flag(&self, flag: Flag) -> bool {
        *self.full & (flag as u8) > 0
    }

    pub fn set_flag(&mut self, flag: Flag, on: bool) {
        *self.full = if on { *self.full | flag as u8 } else { *self.full & !(flag as u8) }
    }
}

impl<'a> From<&'a mut u8> for FlagSetMut<'a> {
    fn from(value: &'a mut u8) -> Self {
        FlagSetMut { full: value }
    }
}
