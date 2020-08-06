use super::cpu::Cpu;
use super::cpu::Register;
use super::cpu::RegisterPair;

pub enum Instruction {
    NOP,
    LD(Argument, Argument),
    STOP,
    JP(Condition, Argument),
    JR(Condition, i8),
    ADD(Argument, Argument),
}

pub enum Argument {
    ImmediateByte(u8),
    ImmediateWord(u16),
    IndirectImmediateByte(u16),
    Register(Register),
    RegisterPair(RegisterPair),
    IndirectRegister(RegisterPair),
}

impl Instruction {
    pub fn from_byte(cpu: &Cpu, byte: u8) -> Instruction {
        if byte == 0xCB {
            Self::from_prefixed_byte(cpu, byte)
        } else {
            Self::from_unprefixed_byte(cpu, byte)
        }
    }

    pub fn from_unprefixed_byte(cpu: &Cpu, byte: u8) -> Instruction {
        // https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        let x = (byte >> 6) & 0b00000011;
        let y = (byte >> 3) & 0b00000111;
        let z = byte & 0b00000111;
        let p = y >> 1;
        let q = y >> 3 & 0b00000001;

        let n = cpu.read_byte(cpu.register_pair(RegisterPair::PC) + 1);
        let nn = cpu.read_word(cpu.register_pair(RegisterPair::PC) + 1);

        match (x, z, q, y, p) {
            (0, 0, _, 0, _) => Instruction::NOP, // NOP
            (0, 0, _, 1, _) => Instruction::LD(
                // LD (nn), SP
                Argument::IndirectImmediateByte(nn),
                Argument::RegisterPair(RegisterPair::SP),
            ),
            (0, 0, _, 2, _) => Instruction::STOP, // STOP
            (0, 0, _, 3, _) => Instruction::JR(Condition::Always, n as i8), // JR d
            (0, 0, _, 4..=7, _) => Instruction::JR(Self::table_cc(y - 4), n as i8), // JR cc[y - 4], d
            (0, 1, 0, _, p) => Instruction::LD(
                // LD rp[p], nn
                Argument::RegisterPair(Self::table_rp(p)),
                Argument::IndirectImmediateByte(nn),
            ),
            (0, 1, 1, _, p) => {
                // ADD HL, rp[p]
                Instruction::ADD(
                    Argument::RegisterPair(RegisterPair::HL),
                    Argument::RegisterPair(Self::table_rp(p)),
                )
            }
            (0, 2, 0, _, 0) => Instruction::LD(
                Argument::IndirectRegister(RegisterPair::BC),
                Argument::Register(Register::A),
            ),
            (0, 2, 0, _, 1) => Instruction::LD(
                Argument::IndirectRegister(RegisterPair::DE),
                Argument::Register(Register::A),
            ),
            (0, 2, 1, _, 0) => Instruction::LD(
                Argument::Register(Register::A),
                Argument::IndirectRegister(RegisterPair::BC),
            ),
            (0, 2, 1, _, 1) => Instruction::LD(
                Argument::Register(Register::A),
                Argument::IndirectRegister(RegisterPair::DE),
            ),
            _ => unreachable!(),
        }
    }

    pub fn from_prefixed_byte(cpu: &Cpu, byte: u8) -> Instruction {
        unimplemented!()
    }

    fn table_rp2(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::AF,
            _ => unreachable!(),
        }
    }

    fn table_rp(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::SP,
            _ => unreachable!(),
        }
    }

    fn table_cc(index: u8) -> Condition {
        match index {
            0 => Condition::NotZero,
            1 => Condition::Zero,
            2 => Condition::NotCarry,
            3 => Condition::Carry,
            _ => unreachable!(),
        }
    }
}

pub enum Condition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}
