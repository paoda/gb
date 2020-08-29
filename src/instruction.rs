use super::cpu::{Cpu, RegisterPair};
pub enum Instruction {
    NOP
}


impl Instruction {
    pub fn from_byte(cpu: &Cpu, byte: u8) -> Self {
        if byte == 0xCB {
            Self::from_prefixed_byte(cpu, byte)
        } else {
            Self::from_unprefixed_byte(cpu, byte)
        }
    }

    fn from_prefixed_byte(cpu: &Cpu, byte: u8) -> Self {
        // https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        let x = (byte >> 6) & 0b00000011;
        let y = (byte >> 3) & 0b00000111;
        let z = byte & 0b00000111;
        let p = y >> 1;
        let q = y & 0b00000001;


        unimplemented!()
    }

    fn from_unprefixed_byte(cpu: &Cpu, byte: u8) -> Self {
        unimplemented!()
    }
}


enum InstrRegisterPairs {
    AF,
    BC,
    DE,
    HL,
    IncrementHL,
    DecrementHL,
}

enum InstrRegisters {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    IndirectHL,
}



enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

struct Table;

impl Table {
    pub fn r(index: u8) -> InstrRegisters {
        match index {
            0 => InstrRegisters::B,
            1 => InstrRegisters::C,
            2 => InstrRegisters::D,
            3 => InstrRegisters::E,
            4 => InstrRegisters::H,
            5 => InstrRegisters::L,
            6 => InstrRegisters::IndirectHL,
            7 => InstrRegisters::A,
            _ => unreachable!("Index {} is out of bounds in r[]", index)
        }
    }

    pub fn rp2(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::AF,
            _ => unreachable!("Index {} out of bounds in rp2[]", index)
        }
    }

    pub fn rp(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::AF,
            _ => unreachable!("Index {} out of bounds in rp[]", index)
        }
    }

    pub fn cc(index: u8) -> JumpCondition {
        match index {
            0 => JumpCondition::NotZero,
            1 => JumpCondition::Zero,
            2 => JumpCondition::NotCarry,
            3 => JumpCondition::Carry,
            _ => unreachable!("Index {} out of bounds in cc[]", index)
        }
    }

    pub fn alu(index: u8) -> Instruction {
        unimplemented!()
    }
}