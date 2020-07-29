use super::instructions::Instruction;

// Gameboy CPU
pub struct LR35902 {
    sp: u16,
    pc: u16,
    reg: Registers,
}

impl LR35902 {
    pub fn cycle(&mut self) {
        let opcode = Self::fetch();
        self.decode(opcode);
        Self::execute();
    }

    fn fetch() -> u8 {
        unimplemented!()
    }

    fn decode(&mut self, opcode: u8) {
        // Source: https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        // x = the opcode's 1st octal digit (i.e. bits 7-6)
        // y = the opcode's 2nd octal digit (i.e. bits 5-3)
        // z = the opcode's 3rd octal digit (i.e. bits 2-0)
        // p = y rightshifted one position (i.e. bits 5-4)
        // q = y modulo 2 (i.e. bit 3)

        let x = opcode >> 6;
        let y = (opcode & 0b00111000) >> 3; // 0b00111000 = 0x38
        let z = opcode & 0b00000111; // 0b00000111 = 0x07
        let p = y >> 1;
        let q = y & 0b00000001; // 0b001 = 0x1;

        let d: i8 = 0; // Displacement Byte
        let n: u8 = 0; // 8-bit Immediate Operand
        let nn: u16 = 0; // 16-bit Immediate Operand

        match (x, z, q, y, p) {
            (0, 0, _, 0, _) => Instruction::nop(),
            _ => panic!("Unexpected Opcode!"),
        }
    }

    fn execute() {
        unimplemented!()
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Registers {
    a: u8, // Accumulator Register
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    f: Flag,
}

impl Registers {
    pub fn set_af(&mut self, value: u16) {
        let (high, low) = Self::split_u16(value);

        self.a = high;
        self.f = low.into();
    }

    pub fn get_af(&self) -> u16 {
        Self::merge_u8s(self.a, self.f.into())
    }

    pub fn set_bc(&mut self, value: u16) {
        let (high, low) = Self::split_u16(value);

        self.b = high;
        self.c = low;
    }

    pub fn get_bc(&self) -> u16 {
        Self::merge_u8s(self.b, self.c)
    }

    pub fn set_de(&mut self, value: u16) {
        let (high, low) = Self::split_u16(value);

        self.d = high;
        self.e = low;
    }

    pub fn get_de(&self) -> u16 {
        Self::merge_u8s(self.d, self.e)
    }

    pub fn set_hl(&mut self, value: u16) {
        let (high, low) = Self::split_u16(value);

        self.h = high;
        self.l = low;
    }

    pub fn get_hl(&self) -> u16 {
        Self::merge_u8s(self.h, self.l)
    }

    pub fn set_a(&mut self, a: u8) {
        self.a = a;
    }

    pub fn get_a(&self) -> u8 {
        self.a
    }

    pub fn set_b(&mut self, b: u8) {
        self.b = b;
    }

    pub fn get_b(&self) -> u8 {
        self.b
    }

    pub fn set_c(&mut self, c: u8) {
        self.c = c;
    }

    pub fn get_c(&self) -> u8 {
        self.c
    }

    pub fn set_d(&mut self, d: u8) {
        self.d = d;
    }

    pub fn get_d(&self) -> u8 {
        self.d
    }

    pub fn set_e(&mut self, e: u8) {
        self.e = e;
    }

    pub fn get_e(&self) -> u8 {
        self.e
    }

    pub fn set_h(&mut self, h: u8) {
        self.h = h;
    }

    pub fn get_h(&self) -> u8 {
        self.h
    }

    pub fn set_l(&mut self, l: u8) {
        self.l = l;
    }

    pub fn get_l(&self) -> u8 {
        self.l
    }

    pub fn set_f<F: Into<Flag>>(&mut self, flag: F) {
        self.f = flag.into();
    }

    pub fn get_f(&self) -> Flag {
        self.f
    }

    pub fn get_u8_f(&self) -> u8 {
        self.f.into()
    }

    fn split_u16(value: u16) -> (u8, u8) {
        ((value >> 8) as u8, value as u8)
    }

    fn merge_u8s(left: u8, right: u8) -> u16 {
        (left as u16) << 8 | right as u16
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Flag(u8);

impl Flag {
    pub fn get_zf(&self) -> bool {
        (self.0 >> 7) == 1
    }

    pub fn set_zf(&mut self, enabled: bool) {
        if enabled {
            self.0 |= 0b10000000; // Set
        } else {
            self.0 &= 0b01111111; // Clear
        }
    }

    pub fn get_n(&self) -> bool {
        ((self.0 >> 6) & 0x01) == 1
    }

    pub fn set_n(&mut self, enabled: bool) {
        if enabled {
            self.0 |= 0b01000000; // Set
        } else {
            self.0 &= 0b10111111; // Clear
        }
    }

    pub fn get_h(&self) -> bool {
        ((self.0 >> 5) & 0x01) == 1
    }

    pub fn set_h(&mut self, enabled: bool) {
        if enabled {
            self.0 |= 0b00100000; // Set
        } else {
            self.0 &= 0b11011111; // Clear
        }
    }

    pub fn get_cy(&self) -> bool {
        ((self.0 >> 4) & 0x01) == 1
    }

    pub fn set_cy(&mut self, enabled: bool) {
        if enabled {
            self.0 |= 0b00010000; // Set
        } else {
            self.0 &= 0b11101111; // Clear
        }
    }
}

impl From<u8> for Flag {
    fn from(value: u8) -> Self {
        Self(value & 0xF0) // Throw out bits 0 -> 3
    }
}

impl From<Flag> for u8 {
    fn from(flag: Flag) -> Self {
        flag.0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn flag_zf_works() {
        let mut flag: Flag = Default::default();

        flag.set_zf(true);
        assert_eq!(u8::from(flag), 0b10000000);
        assert!(flag.get_zf());

        flag.set_zf(false);
        assert_eq!(u8::from(flag), 0b00000000);
        assert!(!flag.get_zf());
    }

    #[test]
    fn flag_n_works() {
        let mut flag: Flag = Default::default();

        flag.set_n(true);
        assert_eq!(u8::from(flag), 0b01000000);
        assert!(flag.get_n());

        flag.set_n(false);
        assert_eq!(u8::from(flag), 0b00000000);
        assert!(!flag.get_n());
    }

    #[test]
    fn flag_h_works() {
        let mut flag: Flag = Default::default();

        flag.set_h(true);
        assert_eq!(u8::from(flag), 0b00100000);
        assert!(flag.get_h());

        flag.set_h(false);
        assert_eq!(u8::from(flag), 0b00000000);
        assert!(!flag.get_h());
    }

    #[test]
    fn flags_work_together() {
        let mut flag: Flag = Default::default();
        assert_eq!(u8::from(flag), 0b00000000);

        flag.set_zf(true);
        flag.set_cy(true);
        flag.set_h(true);
        flag.set_n(true);

        assert_eq!(u8::from(flag), 0b11110000);
        assert!(flag.get_zf());
        assert!(flag.get_n());
        assert!(flag.get_h());
        assert!(flag.get_cy());

        flag.set_cy(false);
        assert_eq!(u8::from(flag), 0b11100000);
        assert!(!flag.get_cy());

        flag.set_n(false);
        assert_eq!(u8::from(flag), 0b10100000);
        assert!(!flag.get_n());
    }

    #[test]
    fn flag_to_u8_and_back_works() {
        let flag: Flag = 0b10011111.into();
        assert_eq!(u8::from(flag), 0b10010000);
        assert_eq!(flag.0, 0b10010000);
        assert!(flag.get_zf());
        assert!(!flag.get_n());
        assert!(!flag.get_h());
        assert!(flag.get_cy());

        let mut base: Flag = Default::default();
        base.set_h(true);
        base.set_zf(true);

        assert_eq!(base.0, 0b10100000);
        assert_eq!(u8::from(base), 0b10100000);
    }

    #[test]
    fn flag_cy_works() {
        let mut flag: Flag = Default::default();

        flag.set_cy(true);
        assert_eq!(u8::from(flag), 0b00010000);
        assert!(flag.get_cy());

        flag.set_cy(false);
        assert_eq!(u8::from(flag), 0b00000000);
        assert!(!flag.get_cy());
    }

    #[test]
    fn split_u16_works() {
        let num = 0xABCD;

        let (high, low) = Registers::split_u16(num);
        assert_eq!(high, 0xAB);
        assert_eq!(low, 0xCD);
    }

    #[test]
    fn merge_u8s_works() {
        let left = 0xAB;
        let right = 0xCD;

        let res = Registers::merge_u8s(left, right);
        assert_eq!(res, 0xABCD);
    }
}
