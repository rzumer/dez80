use std::fmt;

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
pub enum RegisterType {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    IXH,
    IXL,
    IYH,
    IYL,
    I,
    R,
    A_,
    F_,
    B_,
    C_,
    D_,
    E_,
    H_,
    L_,
}

/// Used to identify a register pair in a manner
/// independent of its representation in a `RegisterSet`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegisterPairType {
    AF,
    BC,
    DE,
    HL,
    IX,
    IY,
    PC,
    SP,
    IR,
    AF_,
    BC_,
    DE_,
    HL_,
}

impl_display_register!(RegisterType, RegisterPairType);

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
#[derive(Clone, Copy, Debug, PartialEq)]
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
