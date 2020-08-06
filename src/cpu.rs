use super::bus::MemoryBus;
use super::Instruction;
use std::convert::From;

#[derive(Debug, Copy, Clone)]
pub struct Cpu {
    bus: MemoryBus,
    reg: Registers,
    ime: bool,
    pc: u16,
    sp: u16,
}

impl Cpu {
    pub fn run(&mut self) -> ! {
        loop {
            let opcode = self.fetch();
            let instruction = self.decode(opcode);
            self.execute(instruction);
        }
    }

    fn fetch(&self) -> u8 {
        unimplemented!()
    }

    fn decode(&self, opcode: u8) -> Instruction {
        unimplemented!()
    }

    fn execute(&mut self, instruction: Instruction) {
        unimplemented!()
    }
}

impl Cpu {
    pub fn read_byte(&self, address: u16) -> u8 {
        self.bus.read_byte(address)
    }

    pub fn write_byte(&mut self, address: u16) {
        self.bus.write_byte(address)
    }

    pub fn read_word(&self, address: u16) -> u16 {
        self.bus.read_word(address)
    }

    pub fn write_word(&mut self, address: u16) {
        self.bus.write_word(address)
    }
}

impl Cpu {
    pub fn register_pair(&self, pair: RegisterPair) -> u16 {
        match pair {
            RegisterPair::AF => {
                (self.register(Register::A) as u16) << 8 | self.register(Register::Flag) as u16
            }
            RegisterPair::BC => {
                (self.register(Register::B) as u16) << 8 | self.register(Register::C) as u16
            }
            RegisterPair::DE => {
                (self.register(Register::D) as u16) << 8 | self.register(Register::E) as u16
            }
            RegisterPair::HL => {
                (self.register(Register::H) as u16) << 8 | self.register(Register::L) as u16
            }
            RegisterPair::SP => self.sp,
            RegisterPair::PC => self.pc,
        }
    }

    pub fn set_register_pair(&mut self, pair: RegisterPair, value: u16) {
        let high = (value >> 8) as u8;
        let low = value as u8;

        match pair {
            RegisterPair::AF => {
                self.set_register(Register::A, high);
                self.set_register(Register::Flag, low);
            }
            RegisterPair::BC => {
                self.set_register(Register::B, high);
                self.set_register(Register::C, low);
            }
            RegisterPair::DE => {
                self.set_register(Register::D, high);
                self.set_register(Register::E, low);
            }
            RegisterPair::HL => {
                self.set_register(Register::H, high);
                self.set_register(Register::L, low);
            }
            RegisterPair::SP => self.sp = value,
            RegisterPair::PC => self.pc = value,
        }
    }

    pub fn register(&self, reg: Register) -> u8 {
        match reg {
            Register::A => self.reg.a,
            Register::B => self.reg.b,
            Register::C => self.reg.c,
            Register::D => self.reg.d,
            Register::E => self.reg.e,
            Register::H => self.reg.h,
            Register::L => self.reg.l,
            Register::Flag => self.reg.f.into(),
        }
    }

    pub fn set_register(&mut self, reg: Register, value: u8) {
        match reg {
            Register::A => self.reg.a = value,
            Register::B => self.reg.b = value,
            Register::C => self.reg.c = value,
            Register::D => self.reg.d = value,
            Register::E => self.reg.e = value,
            Register::H => self.reg.h = value,
            Register::L => self.reg.l = value,
            Register::Flag => self.reg.f = value.into(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    f: Flag,
}

#[derive(Debug, Copy, Clone)]
pub enum RegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Copy, Clone)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Flag,
}

#[derive(Debug, Copy, Clone)]
struct Flag {
    z: bool, // Zero Flag
    n: bool, // Negative Flag
    h: bool, // Half-Carry Flag
    c: bool, // Carry Flag
}

impl From<u8> for Flag {
    fn from(flag: u8) -> Self {
        Self {
            z: (flag >> 7) == 1,
            n: ((flag >> 6) & 0x01) == 1,
            h: ((flag >> 5) & 0x01) == 1,
            c: ((flag >> 4) & 0x01) == 1,
        }
    }
}

impl From<Flag> for u8 {
    fn from(flag: Flag) -> Self {
        (flag.z as u8) << 7 | (flag.n as u8) << 6 | (flag.h as u8) << 5 | (flag.c as u8) << 4
    }
}
