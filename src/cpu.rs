use super::bus::MemoryBus;
use std::ops::{Index, IndexMut};
use std::convert::From;

#[derive(Debug, Copy, Clone)]
pub struct LR35902 {
    bus: MemoryBus,
    registers: Registers,
    sp: u16, // Stack Pointer
    pc: u16, // Program Counter
    flag: Flag, // Flag Register
    ime: bool, // Interrupt Master Enable Flag
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
}