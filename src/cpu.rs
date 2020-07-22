// Gameboy CPU
pub struct LR35902 {
    sp: u16,
    pc: u16,
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
    f: Flags,
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

    pub fn set_f<F: Into<Flags>>(&mut self, flag: F) {
        self.f = flag.into();
    }

    pub fn get_f(&self) -> Flags {
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
pub struct Flags {
    zf: bool, // Zero Flag
    n: bool,  // Addition / Subtraction Flag
    h: bool,  // Half Carry Flag
    cy: bool, // Carry Flag
}

impl From<u8> for Flags {
    fn from(value: u8) -> Self {
        Flags {
            zf: (value >> 7) == 1,
            n: (value >> 6) & 0x01 == 1,
            h: (value >> 5) & 0x01 == 1,
            cy: (value >> 4) & 0x01 == 1,
        }
    }
}

impl From<Flags> for u8 {
    fn from(flags: Flags) -> u8 {
        (flags.zf as u8) << 7 | (flags.n as u8) << 6 | (flags.h as u8) << 5 | (flags.cy as u8) << 4
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn u8_to_flags_works() {
        let ex1: Flags = 0b11110000.into();
        assert_eq!(ex1.zf && ex1.n && ex1.h && ex1.cy, true);

        let ex2: Flags = 0b00110000.into();
        assert_eq!((ex2.zf && ex2.n) == false && ex2.h && ex2.cy, true);

        let ex3: Flags = 0b10100000.into();
        assert_eq!((ex3.n && ex3.cy) == false && ex3.zf && ex3.h, true);

        let ex4: Flags = 0b11000000.into();
        assert_eq!((ex4.h && ex4.cy) == false && ex4.zf && ex4.n, true);

        let ex5: Flags = 0b01010000.into();
        assert_eq!((ex5.zf && ex5.h) == false && ex5.n && ex5.cy, true);
    }

    #[test]
    fn flags_to_u8_works() {
        let ex1: u8 = Flags {
            zf: true,
            n: true,
            h: true,
            cy: true,
        }
        .into();
        assert_eq!(ex1, 0b11110000);

        let ex2: u8 = Flags {
            zf: false,
            n: false,
            h: true,
            cy: true,
        }
        .into();
        assert_eq!(ex2, 0b00110000);

        let ex3: u8 = Flags {
            zf: true,
            n: false,
            h: true,
            cy: false,
        }
        .into();
        assert_eq!(ex3, 0b10100000);

        let ex4: u8 = Flags {
            zf: true,
            n: true,
            h: false,
            cy: false,
        }
        .into();
        assert_eq!(ex4, 0b11000000);

        let ex5: u8 = Flags {
            zf: false,
            n: true,
            h: false,
            cy: true,
        }
        .into();
        assert_eq!(ex5, 0b01010000);
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
