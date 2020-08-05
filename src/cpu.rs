use super::bus::MemoryBus;
use std::convert::From;
use std::ops::{Index, IndexMut};

#[derive(Debug, Copy, Clone)]
pub struct LR35902 {
    bus: MemoryBus,
    registers: Registers,
    sp: u16,   // Stack Pointer
    pc: u16,   // Program Counter
    ime: bool, // Interrupt Master Enable Flag
}

impl LR35902 {
    pub fn fetch() -> u8 {
        unimplemented!()
    }

    fn decode(opcode: u8) -> Instruction {
        unimplemented!()
    }

    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Add(target, source) => {
                match target {
                    Argument::Register(reg) => {
                        let right = match source {
                            Argument::Register(other) => self.registers[other], // ADD A, r
                            Argument::ImmediateByte(byte) => byte,              // ADD A, n
                            Argument::IndirectRegisterPair(pair) => {
                                // ADD A, (HL)
                                self.bus.read_byte(self.register_pair(pair))
                            }
                            _ => unreachable!(),
                        };

                        self.registers[reg] = self.add(self.registers[reg], right);
                    }
                    Argument::RegisterPair(pair) => {
                        let left = self.register_pair(pair);

                        if let Argument::RegisterPair(other) = source {
                            // ADD HL, rr
                            let sum = self.add_u16(left, self.register_pair(other));
                            self.set_register_pair(pair, sum);
                        } else if let Argument::ImmediateByte(byte) = source {
                            // ADD SP, dd
                            let byte = byte as i8;
                            let sum = self.add_u16_i8(left, byte);
                            self.set_register_pair(pair, sum);
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unimplemented!("Unhandled Instruction {:?}", instruction),
        }
    }

    fn add_u16_i8(&mut self, left: u16, right: i8) -> u16 {
        unimplemented!()
    }

    fn add_u16(&mut self, left: u16, right: u16) -> u16 {
        unimplemented!()
    }

    fn add(&mut self, left: u8, right: u8) -> u8 {
        // Info on the Half-Carry flag
        // https://robdor.com/2016/08/10/gameboy-emulator-half-carry-flag/

        let (res, did_overflow) = left.overflowing_add(right);
        self.registers.f = Flag {
            z: res == 0,
            n: false,
            h: (((left & 0x0F) + (right & 0x0F)) & 0x10) == 0x10,
            c: did_overflow,
        }
        .into();

        res
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Flag {
    z: bool, // Zero Flag
    n: bool, // Subtract Flag
    h: bool, // Half-Carry Flag
    c: bool, // Carry Flag
}

impl From<u8> for Flag {
    fn from(num: u8) -> Self {
        Flag {
            z: (num >> 7) == 1,
            n: ((num >> 6) & 0x01) == 1,
            h: ((num >> 5) & 0x01) == 1,
            c: ((num >> 4) & 0x01) == 1,
        }
    }
}

impl From<Flag> for u8 {
    fn from(flag: Flag) -> Self {
        (flag.z as u8) << 7 | (flag.n as u8) << 6 | (flag.h as u8) << 5 | (flag.c as u8) << 4
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    f: u8,
}

impl Index<Register> for Registers {
    type Output = u8;

    fn index(&self, index: Register) -> &Self::Output {
        match index {
            Register::A => &self.a,
            Register::B => &self.b,
            Register::C => &self.c,
            Register::D => &self.d,
            Register::E => &self.e,
            Register::H => &self.h,
            Register::L => &self.l,
            Register::F => &self.f,
        }
    }
}

impl IndexMut<Register> for Registers {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        match index {
            Register::A => &mut self.a,
            Register::B => &mut self.b,
            Register::C => &mut self.c,
            Register::D => &mut self.d,
            Register::E => &mut self.e,
            Register::H => &mut self.h,
            Register::L => &mut self.l,
            Register::F => &mut self.f,
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
    F,
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

impl LR35902 {
    pub fn register_pair(&self, pair: RegisterPair) -> u16 {
        match pair {
            RegisterPair::AF => {
                (self.registers[Register::A] as u16) << 8 | self.registers[Register::F] as u16
            }
            RegisterPair::BC => {
                (self.registers[Register::B] as u16) << 8 | self.registers[Register::C] as u16
            }
            RegisterPair::DE => {
                (self.registers[Register::D] as u16) << 8 | self.registers[Register::E] as u16
            }
            RegisterPair::HL => {
                (self.registers[Register::H] as u16) << 8 | self.registers[Register::L] as u16
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
                self.registers[Register::A] = high;
                self.registers[Register::F] = low;
            }
            RegisterPair::BC => {
                self.registers[Register::B] = high;
                self.registers[Register::C] = low;
            }
            RegisterPair::DE => {
                self.registers[Register::D] = high;
                self.registers[Register::E] = low;
            }
            RegisterPair::HL => {
                self.registers[Register::H] = high;
                self.registers[Register::L] = low;
            }
            RegisterPair::SP => self.sp = value,
            RegisterPair::PC => self.pc = value,
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Argument {
    Register(Register),
    RegisterPair(RegisterPair),
    IndirectRegisterPair(RegisterPair),
    ImmediateByte(u8),
    ImmediateWord(u16),
    IndirectImmediateByte(u16),
}

#[derive(Debug, Copy, Clone)]
enum Instruction {
    Add(Argument, Argument),
    NOP,
    STOP,
    HALT,
}
