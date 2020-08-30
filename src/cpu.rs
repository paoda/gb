use super::bus::Bus;
use super::instruction::Instruction;
pub struct Cpu {
    bus: Bus,
    reg: Registers,
    flags: Flags,
    ime: bool,
    state: State,

}

impl Cpu {
    pub fn fetch(&self) -> u8 {
        self.bus.read_byte(self.reg.pc)
        // TODO: Figure out where to increment the program counter.
    }

    pub fn decode(&self, opcode: u8) -> Instruction {
        Instruction::from_byte(self, opcode)
    }

    pub fn execute(&mut self, instruction: Instruction) {
        unimplemented!()
    }
}


impl Cpu {
    pub fn read_byte(&self, address: u16) -> u8 {
        self.bus.read_byte(address)
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        self.bus.write_byte(address, byte)
    }

    pub fn read_word(&self, address: u16) -> u16 {
        self.bus.read_word(address)
    }

    pub fn write_word(&mut self, address: u16, word: u16) {
        self.bus.write_word(address, word)
    }
}

enum State {
    Execute,
    Halt,
    Stop,
}


impl Cpu {
    pub fn set_register(&mut self, register: Register, value: u8) {
        match register {
            Register::A => self.reg.a = value,
            Register::B => self.reg.b = value,
            Register::C => self.reg.c = value,
            Register::D => self.reg.d = value,
            Register::E => self.reg.e = value,
            Register::H => self.reg.h = value,
            Register::L => self.reg.l = value,
            Register::Flags => self.reg.a = value.into()
        }
    }

    pub fn register(&self, register: Register) -> u8 {
        match register {
            Register::A => self.reg.a,
            Register::B => self.reg.b,
            Register::C => self.reg.c,
            Register::D => self.reg.d,
            Register::E => self.reg.e,
            Register::H => self.reg.h,
            Register::L => self.reg.l,
            Register::Flags => self.flags.into()
        }
    }

    pub fn register_pair(&self, pair: RegisterPair) -> u16 {
        match pair {
            RegisterPair::AF => (self.reg.a as u16) << 8 | u8::from(self.flags) as u16,
            RegisterPair::BC => (self.reg.b as u16) << 8 | self.reg.c as u16,
            RegisterPair::DE => (self.reg.d as u16) << 8 | self.reg.e as u16,
            RegisterPair::HL => (self.reg.h as u16) << 8 | self.reg.l as u16,
            RegisterPair::SP => self.reg.sp,
            RegisterPair::PC => self.reg.pc,
        }
    }

    pub fn set_register_pair(&mut self, pair: RegisterPair, value: u16) {
        let high = (value >> 8) as u8;
        let low = value as u8;
        
        match pair {
            RegisterPair::AF => {
                self.reg.a = high;
                self.flags = low.into();
            }
            RegisterPair::BC => {
                self.reg.b = high;
                self.reg.c = low;
            }
            RegisterPair::DE => {
                self.reg.d = high;
                self.reg.e = low;
            }
            RegisterPair::HL => {
                self.reg.h = high;
                self.reg.l = low;
            }
            RegisterPair::SP => self.reg.sp = value,
            RegisterPair::PC => self.reg.pc = value,
        }
    }
}



pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Flags,
}

pub enum RegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}


struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
}


#[derive(Debug, Copy, Clone)]
struct Flags {
    z: bool, // Zero Flag
    n: bool, // Negative Flag
    h: bool, // Half-Carry Flag
    c: bool, // Carry Flag
}

impl From<u8> for Flags {
    fn from(flag: u8) -> Self {
        Self {
            z: (flag >> 7) == 1,
            n: ((flag >> 6) & 0x01) == 1,
            h: ((flag >> 5) & 0x01) == 1,
            c: ((flag >> 4) & 0x01) == 1,
        }
    }
}

impl From<Flags> for u8 {
    fn from(flag: Flags) -> Self {
        (flag.z as u8) << 7 | (flag.n as u8) << 6 | (flag.h as u8) << 5 | (flag.c as u8) << 4
    }
}
