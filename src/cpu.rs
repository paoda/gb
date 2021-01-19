use super::bus::Bus;
use super::instruction::{Cycles, Instruction};
use super::ppu::PPU;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone, Default)]
pub struct Cpu {
    pub bus: Bus,
    reg: Registers,
    flags: Flags,
    ime: bool,
    state: State,
}

impl Cpu {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_without_boot() -> Self {
        Self {
            bus: Bus::without_boot(),
            reg: Registers {
                a: 0x01,
                b: 0x00,
                c: 0x13,
                d: 0x00,
                e: 0xD8,
                h: 0x01,
                l: 0x4D,
                sp: 0xFFFE,
                pc: 0x0100,
            },
            flags: 0xb0.into(),
            ..Default::default()
        }
    }

    pub fn ime(&self) -> bool {
        self.ime
    }

    pub fn set_ime(&mut self, enabled: bool) {
        self.ime = enabled;
    }

    pub fn inc_pc(&mut self) {
        self.reg.pc += 1;
    }

    pub fn load_cartridge(&mut self, path: &str) {
        self.bus.load_cartridge(path);
    }
}

impl Cpu {
    pub fn fetch(&mut self) -> u8 {
        let opcode = self.bus.read_byte(self.reg.pc);
        self.inc_pc();

        opcode
    }

    pub fn decode(&mut self, opcode: u8) -> Instruction {
        Instruction::from_byte(self, opcode)
    }

    pub fn execute(&mut self, instruction: Instruction) -> Cycles {
        Instruction::execute(self, instruction)
    }

    pub fn step(&mut self) -> Cycles {
        let opcode = self.fetch();
        let instr = self.decode(opcode);
        let cycles = self.execute(instr);

        println!(
            "Addr: {:#06X} | Opcode: {:#04X} | Instr: {:X?}",
            self.reg.pc, opcode, instr
        );

        self.bus.step(cycles);

        cycles
    }
}

impl Cpu {
    pub fn read_imm_byte(&mut self, addr: u16) -> u8 {
        self.inc_pc();
        self.bus.read_byte(addr)
    }

    pub fn read_imm_word(&mut self, addr: u16) -> u16 {
        self.inc_pc();
        self.inc_pc();
        self.bus.read_word(addr)
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        self.bus.read_byte(addr)
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.bus.write_byte(addr, byte);
    }

    pub fn read_word(&mut self, addr: u16) -> u16 {
        self.bus.read_word(addr)
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        self.bus.write_word(addr, word)
    }
}

impl Cpu {
    pub fn get_ppu(&mut self) -> &mut PPU {
        &mut self.bus.ppu
    }
}

#[derive(Debug, Copy, Clone)]
enum State {
    Execute,
    // Halt,
    // Stop,
}

impl Default for State {
    fn default() -> Self {
        Self::Execute
    }
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
            Register::Flag => self.flags = value.into(),
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
            Register::Flag => self.flags.into(),
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
pub enum RegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Copy, Clone, Default)]
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

#[derive(Debug, Copy, Clone, Default)]
pub struct Flags {
    pub z: bool, // Zero Flag
    pub n: bool, // Negative Flag
    pub h: bool, // Half-Carry Flag
    pub c: bool, // Carry Flag
}

impl Flags {
    pub fn update(&mut self, z: bool, n: bool, h: bool, c: bool) {
        self.z = z;
        self.n = n;
        self.h = h;
        self.c = c;
    }
}

impl Display for Flags {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if self.z {
            f.write_str("Z")?;
        } else {
            f.write_str("_")?;
        }

        if self.n {
            f.write_str("N")?;
        } else {
            f.write_str("_")?;
        }

        if self.h {
            f.write_str("H")?;
        } else {
            f.write_str("_")?;
        }

        if self.c {
            f.write_str("C")
        } else {
            f.write_str("_")
        }
    }
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
