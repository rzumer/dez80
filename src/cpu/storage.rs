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
    X,
    Y,
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

    pub fn full(&self) -> u16 {
        (self.high as u16) << 8 | self.low as u16
    }
    pub fn set_full(&mut self, value: u16) {
        self.high = (value >> 8) as u8;
        self.low = value as u8;
    }
}

/// Represents individual Z80 status flags.
#[repr(u8)]
pub enum Flag {
    S = 0b10000000,
    Z = 0b01000000,
    F5 = 0b00100000,
    H = 0b00010000,
    F3 = 0b00001000,
    PV = 0b00000100,
    N = 0b00000010,
    C = 0b00000001,
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
    pub full: &'a mut Register,
}

impl<'a> FlagSet<'a> {
    pub fn flag(&self, flag: Flag) -> bool {
        *self.full & (flag as u8) > 0
    }
    pub fn set_flag(&mut self, flag: Flag, on: bool) {
        *self.full = if on {
            *self.full | flag as u8
        } else {
            *self.full & !(flag as u8)
        }
    }
}

impl<'a> From<&'a mut u8> for FlagSet<'a> {
    fn from(value: &'a mut u8) -> Self {
        FlagSet { full: value }
    }
}

/// Represents the standard set of registers in a Z80 CPU.
///
/// Provides interfaces to read and write any register or
/// register pair available.
#[derive(Default)]
pub struct RegisterSet {
    af: RegisterPair,
    bc: RegisterPair,
    de: RegisterPair,
    hl: RegisterPair,
    ix: RegisterPair,
    iy: RegisterPair,
    pc: RegisterPair,
    sp: RegisterPair,
    ir: RegisterPair,
    af_: RegisterPair,
    bc_: RegisterPair,
    de_: RegisterPair,
    hl_: RegisterPair,
}

impl RegisterSet {
    pub fn flags(&mut self) -> FlagSet {
        (&mut self.af.low).into()
    }

    pub fn read(&self, register: RegisterType) -> u8 {
        use self::RegisterType::*;

        match register {
            A => self.af.high,
            F => self.af.low,
            B => self.bc.high,
            C => self.bc.low,
            D => self.de.high,
            E => self.de.low,
            H => self.hl.high,
            L => self.hl.low,
            X => self.ix.low,
            Y => self.iy.low,
            I => self.ir.high,
            R => self.ir.low,
            A_ => self.af_.high,
            F_ => self.af_.low,
            B_ => self.bc_.high,
            C_ => self.bc_.low,
            D_ => self.de_.high,
            E_ => self.de_.low,
            H_ => self.hl_.high,
            L_ => self.hl_.low,
        }
    }

    pub fn read_pair(&self, register_pair: RegisterPairType) -> u16 {
        use self::RegisterPairType::*;

        match register_pair {
            AF => self.af.full(),
            BC => self.bc.full(),
            DE => self.de.full(),
            HL => self.hl.full(),
            IX => self.ix.full(),
            IY => self.iy.full(),
            PC => self.pc.full(),
            SP => self.sp.full(),
            IR => self.ir.full(),
            AF_ => self.af_.full(),
            BC_ => self.bc_.full(),
            DE_ => self.de_.full(),
            HL_ => self.hl_.full(),
        }
    }

    pub fn write(&mut self, register: RegisterType, value: u8) {
        use self::RegisterType::*;

        match register {
            A => self.af.high = value,
            F => self.af.low = value,
            B => self.bc.high = value,
            C => self.bc.low = value,
            D => self.de.high = value,
            E => self.de.low = value,
            H => self.hl.high = value,
            L => self.hl.low = value,
            X => self.ix.low = value,
            Y => self.iy.low = value,
            I => self.ir.high = value,
            R => self.ir.low = value,
            A_ => self.af_.high = value,
            F_ => self.af_.low = value,
            B_ => self.bc_.high = value,
            C_ => self.bc_.low = value,
            D_ => self.de_.high = value,
            E_ => self.de_.low = value,
            H_ => self.hl_.high = value,
            L_ => self.hl_.low = value,
        }
    }

    pub fn write_pair(&mut self, register_pair: RegisterPairType, value: u16) {
        use self::RegisterPairType::*;

        match register_pair {
            AF => self.af.set_full(value),
            BC => self.bc.set_full(value),
            DE => self.de.set_full(value),
            HL => self.hl.set_full(value),
            IX => self.ix.set_full(value),
            IY => self.iy.set_full(value),
            PC => self.pc.set_full(value),
            SP => self.sp.set_full(value),
            IR => self.ir.set_full(value),
            AF_ => self.af_.set_full(value),
            BC_ => self.bc_.set_full(value),
            DE_ => self.de_.set_full(value),
            HL_ => self.hl_.set_full(value),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_full_register_pair() {
        let mut r = RegisterPair::new(0);
        r.set_full(0xF00F);
        assert_eq!(0xF0, r.high);
        assert_eq!(0x0F, r.low);
    }

    #[test]
    fn set_high_register_pair() {
        let mut r = RegisterPair::new(0);
        r.high = 0x0F;
        assert_eq!(0x0F00, r.full());
    }

    #[test]
    fn set_low_register_pair() {
        let mut r = RegisterPair::new(0);
        r.low = 0xF0;
        assert_eq!(0x00F0, r.full());
    }

    #[test]
    fn set_disjoint_flags() {
        let mut flag_data = 0_u8;
        let flags: FlagSet = (&mut flag_data).into();
        *flags.full = 0b10101010;
        assert_eq!(true, flags.flag(Flag::S));
        assert_eq!(false, flags.flag(Flag::Z));
        assert_eq!(true, flags.flag(Flag::F5));
        assert_eq!(false, flags.flag(Flag::H));
        assert_eq!(true, flags.flag(Flag::F3));
        assert_eq!(false, flags.flag(Flag::PV));
        assert_eq!(true, flags.flag(Flag::N));
        assert_eq!(false, flags.flag(Flag::C));
    }

    #[test]
    fn set_overlapping_flag() {
        let mut flag_data = 0_u8;
        let mut flags: FlagSet = (&mut flag_data).into();
        flags.set_flag(Flag::PARITY, true);
        assert_eq!(Flag::OVERFLOW as u8, *flags.full);
    }

    #[test]
    fn set_register_set_flags() {
        let mut registers = RegisterSet::default();
        registers.write(RegisterType::F, 0xF0);

        let mut flags = registers.flags();
        flags.set_flag(Flag::CARRY, true);
        assert_eq!(0xF0 | Flag::CARRY as u8, registers.af.low);
    }

    #[test]
    fn read_af() {
        let mut registers = RegisterSet::default();
        registers.af.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::A));
        assert_eq!(0x0F, registers.read(RegisterType::F));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::AF));
    }

    #[test]
    fn read_bc() {
        let mut registers = RegisterSet::default();
        registers.bc.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::B));
        assert_eq!(0x0F, registers.read(RegisterType::C));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::BC));
    }

    #[test]
    fn read_de() {
        let mut registers = RegisterSet::default();
        registers.de.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::D));
        assert_eq!(0x0F, registers.read(RegisterType::E));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::DE));
    }

    #[test]
    fn read_hl() {
        let mut registers = RegisterSet::default();
        registers.hl.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::H));
        assert_eq!(0x0F, registers.read(RegisterType::L));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::HL));
    }

    #[test]
    fn read_ix() {
        let mut registers = RegisterSet::default();
        registers.ix.set_full(0xF00F);

        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::IX));
    }

    #[test]
    fn read_iy() {
        let mut registers = RegisterSet::default();
        registers.iy.set_full(0xF00F);

        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::IY));
    }

    #[test]
    fn read_pc() {
        let mut registers = RegisterSet::default();
        registers.pc.set_full(0xF00F);

        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::PC));
    }

    #[test]
    fn read_sp() {
        let mut registers = RegisterSet::default();
        registers.sp.set_full(0xF00F);

        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::SP));
    }

    #[test]
    fn read_ir() {
        let mut registers = RegisterSet::default();
        registers.ir.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::I));
        assert_eq!(0x0F, registers.read(RegisterType::R));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::IR));
    }

    #[test]
    fn read_af_() {
        let mut registers = RegisterSet::default();
        registers.af_.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::A_));
        assert_eq!(0x0F, registers.read(RegisterType::F_));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::AF_));
    }

    #[test]
    fn read_bc_() {
        let mut registers = RegisterSet::default();
        registers.bc_.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::B_));
        assert_eq!(0x0F, registers.read(RegisterType::C_));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::BC_));
    }

    #[test]
    fn read_de_() {
        let mut registers = RegisterSet::default();
        registers.de_.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::D_));
        assert_eq!(0x0F, registers.read(RegisterType::E_));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::DE_));
    }

    #[test]
    fn read_hl_() {
        let mut registers = RegisterSet::default();
        registers.hl_.set_full(0xF00F);

        assert_eq!(0xF0, registers.read(RegisterType::H_));
        assert_eq!(0x0F, registers.read(RegisterType::L_));
        assert_eq!(0xF00F, registers.read_pair(RegisterPairType::HL_));
    }

    #[test]
    fn write_af() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::A, 0xF0);
        registers.write(RegisterType::F, 0x0F);
        assert_eq!(0xF00F, registers.af.full());

        registers.write_pair(RegisterPairType::AF, 0x0FF0);
        assert_eq!(0x0FF0, registers.af.full());
    }

    #[test]
    fn write_bc() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::B, 0xF0);
        registers.write(RegisterType::C, 0x0F);
        assert_eq!(0xF00F, registers.bc.full());

        registers.write_pair(RegisterPairType::BC, 0x0FF0);
        assert_eq!(0x0FF0, registers.bc.full());
    }

    #[test]
    fn write_de() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::D, 0xF0);
        registers.write(RegisterType::E, 0x0F);
        assert_eq!(0xF00F, registers.de.full());

        registers.write_pair(RegisterPairType::DE, 0x0FF0);
        assert_eq!(0x0FF0, registers.de.full());
    }

    #[test]
    fn write_hl() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::H, 0xF0);
        registers.write(RegisterType::L, 0x0F);
        assert_eq!(0xF00F, registers.hl.full());

        registers.write_pair(RegisterPairType::HL, 0x0FF0);
        assert_eq!(0x0FF0, registers.hl.full());
    }

    #[test]
    fn write_ix() {
        let mut registers = RegisterSet::default();

        registers.write_pair(RegisterPairType::IX, 0xF00F);
        assert_eq!(0xF00F, registers.ix.full());
    }

    #[test]
    fn write_iy() {
        let mut registers = RegisterSet::default();

        registers.write_pair(RegisterPairType::IY, 0xF00F);
        assert_eq!(0xF00F, registers.iy.full());
    }

    #[test]
    fn write_pc() {
        let mut registers = RegisterSet::default();

        registers.write_pair(RegisterPairType::PC, 0xF00F);
        assert_eq!(0xF00F, registers.pc.full());
    }

    #[test]
    fn write_sp() {
        let mut registers = RegisterSet::default();

        registers.write_pair(RegisterPairType::SP, 0xF00F);
        assert_eq!(0xF00F, registers.sp.full());
    }

    #[test]
    fn write_ir() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::I, 0xF0);
        registers.write(RegisterType::R, 0x0F);
        assert_eq!(0xF00F, registers.ir.full());

        registers.write_pair(RegisterPairType::IR, 0x0FF0);
        assert_eq!(0x0FF0, registers.ir.full());
    }

    #[test]
    fn write_af_() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::A_, 0xF0);
        registers.write(RegisterType::F_, 0x0F);
        assert_eq!(0xF00F, registers.af_.full());

        registers.write_pair(RegisterPairType::AF_, 0x0FF0);
        assert_eq!(0x0FF0, registers.af_.full());
    }

    #[test]
    fn write_bc_() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::B_, 0xF0);
        registers.write(RegisterType::C_, 0x0F);
        assert_eq!(0xF00F, registers.bc_.full());

        registers.write_pair(RegisterPairType::BC_, 0x0FF0);
        assert_eq!(0x0FF0, registers.bc_.full());
    }

    #[test]
    fn write_de_() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::D_, 0xF0);
        registers.write(RegisterType::E_, 0x0F);
        assert_eq!(0xF00F, registers.de_.full());

        registers.write_pair(RegisterPairType::DE_, 0x0FF0);
        assert_eq!(0x0FF0, registers.de_.full());
    }

    #[test]
    fn write_hl_() {
        let mut registers = RegisterSet::default();

        registers.write(RegisterType::H_, 0xF0);
        registers.write(RegisterType::L_, 0x0F);
        assert_eq!(0xF00F, registers.hl_.full());

        registers.write_pair(RegisterPairType::HL_, 0x0FF0);
        assert_eq!(0x0FF0, registers.hl_.full());
    }
}
