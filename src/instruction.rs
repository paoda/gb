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
    LDD(Argument, Argument),
    LDI(Argument, Argument),
    INC(AllRegisters),
    DEC(AllRegisters),
    RLCA,
    RRCA,
    RLA,
    RRA,
    DAA,
    CPL,
    SCF,
    CCF,
    HALT,
}

pub enum AllRegisters {
    U8(Register),
    U16(RegisterPair),
    IndirectHL,
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
                // LD (BC), A
                Argument::IndirectRegister(RegisterPair::BC),
                Argument::Register(Register::A),
            ),
            (0, 2, 0, _, 1) => Instruction::LD(
                // LD (DE), A
                Argument::IndirectRegister(RegisterPair::DE),
                Argument::Register(Register::A),
            ),
            (0, 2, 1, _, 0) => Instruction::LD(
                // LD A, (BC)
                Argument::Register(Register::A),
                Argument::IndirectRegister(RegisterPair::BC),
            ),
            (0, 2, 1, _, 1) => Instruction::LD(
                // LD A, (DE)
                Argument::Register(Register::A),
                Argument::IndirectRegister(RegisterPair::DE),
            ),
            (0, 2, 0, _, 2) => Instruction::LDI(
                // LD (HL+), A
                Argument::IndirectRegister(RegisterPair::HL),
                Argument::Register(Register::A),
            ),
            (0, 2, 0, _, 3) => Instruction::LDD(
                // LD (HL-), A
                Argument::IndirectRegister(RegisterPair::HL),
                Argument::Register(Register::A),
            ),
            (0, 2, 1, _, 2) => Instruction::LDI(
                // LD A, (HL+)
                Argument::Register(Register::A),
                Argument::IndirectRegister(RegisterPair::HL),
            ),
            (0, 2, 1, _, 3) => Instruction::LDD(
                // LD A, (HL-)
                Argument::Register(Register::A),
                Argument::IndirectRegister(RegisterPair::HL),
            ),
            (0, 3, 0, _, p) => Instruction::INC(AllRegisters::U16(Self::table_rp(p))), // INC rp[p]
            (0, 3, 1, _, p) => Instruction::DEC(AllRegisters::U16(Self::table_rp(p))), // DEC rp[p]
            (0, 4, _, y, _) => Instruction::INC(Self::table_r(y)),                     // INC r[y]
            (0, 5, _, y, _) => Instruction::DEC(Self::table_r(y)),                     // DEC r[y]
            (0, 6, _, y, _) => Instruction::LD(
                // LD r[y], n
                Self::ld_table_r(y),
                Argument::ImmediateByte(n),
            ),
            (0, 7, _, 0, _) => Instruction::RLCA,
            (0, 7, _, 1, _) => Instruction::RRCA,
            (0, 7, _, 2, _) => Instruction::RLA,
            (0, 7, _, 3, _) => Instruction::RRA,
            (0, 7, _, 4, _) => Instruction::DAA,
            (0, 7, _, 5, _) => Instruction::CPL,
            (0, 7, _, 6, _) => Instruction::SCF,
            (0, 7, _, 7, _) => Instruction::CCF,
            (1, 6, _, 6, _) => Instruction::HALT,
            (1, z, _, y, _) => Instruction::LD(
                // LD r[y], r[z]
                Self::ld_table_r(y),
                Self::ld_table_r(z),
            ),
            _ => unreachable!(),
        }
    }

    pub fn from_prefixed_byte(cpu: &Cpu, byte: u8) -> Instruction {
        unimplemented!()
    }

    fn ld_table_r(index: u8) -> Argument {
        match Self::table_r(index) {
            AllRegisters::U8(register) => Argument::Register(register),
            AllRegisters::IndirectHL => Argument::IndirectRegister(RegisterPair::HL),
            _ => unreachable!(),
        }
    }

    fn table_r(index: u8) -> AllRegisters {
        match index {
            0 => AllRegisters::U8(Register::B),
            1 => AllRegisters::U8(Register::C),
            2 => AllRegisters::U8(Register::D),
            3 => AllRegisters::U8(Register::E),
            4 => AllRegisters::U8(Register::H),
            5 => AllRegisters::U8(Register::L),
            6 => AllRegisters::IndirectHL,
            7 => AllRegisters::U8(Register::A),
            _ => unreachable!(),
        }
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
