use super::cpu::{Cpu, RegisterPair};
pub enum Instruction {
    NOP,
    LD(LDTarget, LDTarget),
    STOP,
    JR(JumpCondition, i8),
    ADD(MATHTarget, MATHTarget),
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
    ADC(MATHTarget, MATHTarget),
    SUB(MATHTarget), // SUB A, MATHTarget always
    SBC(MATHTarget, MATHTarget),
    AND(MATHTarget), // AND A, MATHTarget always
    XOR(MATHTarget), // XOR A, MATHTarget always
    OR(MATHTarget),  // OR A, MATHTarget always
    CP(MATHTarget),  // CP A, MATHTarget always
    RET(JumpCondition),
    LDHL(i8), // LD HL, SP + d
    POP(RegisterPair),
    RETI,
    JP(JumpCondition, JPTarget),
    DI,
    EI,
    CALL(JumpCondition, u16),
    PUSH(RegisterPair),
    RST(u8),
}

impl Instruction {
    pub fn from_byte(cpu: &Cpu, byte: u8) -> Self {
        if byte == 0xCB {
            Self::from_prefixed_byte(cpu, byte)
        } else {
            Self::from_unprefixed_byte(cpu, byte)
        }
    }

    fn from_prefixed_byte(cpu: &Cpu, opcode: u8) -> Self {
        // https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        let x = (opcode >> 6) & 0b00000011;
        let y = (opcode >> 3) & 0b00000111;
        let z = opcode & 0b00000111;
        let p = y >> 1;
        let q = y & 0b00000001;

        let n = cpu.read_byte(cpu.register_pair(RegisterPair::PC) + 1);
        let nn = cpu.read_word(cpu.register_pair(RegisterPair::PC) + 1);

        match (x, z, q, y, p) {
            (0, 0, _, 0, _) => Instruction::NOP, // NOP
            (0, 0, _, 1, _) => Instruction::LD(
                // LD (nn), SP
                LDTarget::ByteAtAddress(nn),
                LDTarget::RegisterPair(RegisterPair::SP),
            ),
            (0, 0, _, 2, _) => Instruction::STOP, // STOP
            (0, 0, _, 3, _) => Instruction::JR(JumpCondition::Always, n as i8), // JR d
            (0, 0, _, 4..=7, _) => Instruction::JR(Table::cc(y - 4), n as i8), // JR cc[y - 4], d
            (0, 1, 0, _, _) => Instruction::LD(
                // LD rp[p], nn
                LDTarget::RegisterPair(Table::rp(p)),
                LDTarget::ImmediateWord(nn),
            ),
            (0, 1, 1, _, _) => Instruction::ADD(
                // ADD HL, rp[p]
                MATHTarget::HL,
                MATHTarget::RegisterPair(Table::rp(p)),
            ),
            (0, 2, 0, _, 0) => Instruction::LD(
                // LD (BC), A
                LDTarget::IndirectRegister(InstrRegisterPairs::BC),
                LDTarget::Register(InstrRegisters::A),
            ),
            (0, 2, 0, _, 1) => Instruction::LD(
                // LD (DE), A
                LDTarget::IndirectRegister(InstrRegisterPairs::DE),
                LDTarget::Register(InstrRegisters::A),
            ),
            (0, 2, 1, _, 0) => Instruction::LD(
                // LD A, (BC)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::IndirectRegister(InstrRegisterPairs::BC),
            ),
            (0, 2, 1, _, 1) => Instruction::LD(
                // LD A, (DE)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::IndirectRegister(InstrRegisterPairs::DE),
            ),
            (0, 2, 0, _, 2) => Instruction::LD(
                // LD (HL+), A
                LDTarget::IndirectRegister(InstrRegisterPairs::IncrementHL),
                LDTarget::Register(InstrRegisters::A),
            ),
            (0, 2, 0, _, 3) => Instruction::LD(
                // LD (HL-), A
                LDTarget::IndirectRegister(InstrRegisterPairs::DecrementHL),
                LDTarget::Register(InstrRegisters::A),
            ),
            (0, 2, 1, _, 2) => Instruction::LD(
                // LD A, (HL+)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::IndirectRegister(InstrRegisterPairs::IncrementHL),
            ),
            (0, 2, 1, _, 3) => Instruction::LD(
                // LD A, (HL-)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::IndirectRegister(InstrRegisterPairs::DecrementHL),
            ),
            (0, 3, 0, _, _) => Instruction::INC(
                // INC rp[p]
                AllRegisters::Word(Table::rp(p)),
            ),
            (0, 3, 1, _, _) => Instruction::DEC(
                // DEC rp[p]
                AllRegisters::Word(Table::rp(p)),
            ),
            (0, 4, _, _, _) => Instruction::INC(
                // INC r[y]
                AllRegisters::Byte(Table::r(y)),
            ),
            (0, 5, _, _, _) => Instruction::DEC(
                // DEC r[y]
                AllRegisters::Byte(Table::r(y)),
            ),
            (0, 6, _, _, _) => Instruction::LD(
                // LD r[y], n
                LDTarget::Register(Table::r(y)),
                LDTarget::ImmediateByte(n),
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
            (1, _, _, _, _) => Instruction::LD(
                // LD r[y], r[z]
                LDTarget::Register(Table::r(y)),
                LDTarget::Register(Table::r(z)),
            ),
            (2, _, _, _, _) => Table::x2_alu(y, z), // alu[y] r[z]
            (3, 0, _, 0..=3, _) => Instruction::RET(Table::cc(y)), // RET cc[y]
            (3, 0, _, 4, _) => Instruction::LD(
                // LD (0xFF00 + n), A
                LDTarget::ByteAtAddress(0xFF00 + (n as u16)), // TODO: Do we want to do any calculations here?
                LDTarget::Register(InstrRegisters::A),
            ),
            (3, 0, _, 5, _) => Instruction::ADD(
                // ADD SP, d
                MATHTarget::RegisterPair(RegisterPair::SP),
                MATHTarget::ImmediateByte(n),
            ),
            (3, 0, _, 6, _) => Instruction::LD(
                // LD A, (0xFF00 + n)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::ByteAtAddress(0xFF00 + (n as u16)), // TODO: DO we want to do any calculations here?
            ),
            (3, 0, _, 7, _) => Instruction::LDHL(n as i8), // LD HL, SP + d
            (3, 1, 0, _, _) => Instruction::POP(Table::rp2(p)), // POP rp2[p]
            (3, 1, 1, _, 0) => Instruction::RET(JumpCondition::Always), // RET
            (3, 1, 1, _, 1) => Instruction::RETI,
            (3, 1, 1, _, 2) => Instruction::JP(
                // JP HL
                JumpCondition::Always,
                JPTarget::RegisterPair(RegisterPair::HL),
            ),
            (3, 1, 1, _, 3) => Instruction::LD(
                // LD SP, HL
                LDTarget::RegisterPair(RegisterPair::SP),
                LDTarget::RegisterPair(RegisterPair::HL),
            ),
            (3, 2, _, 0..=3, _) => Instruction::JP(
                // JP cc[y], nn
                Table::cc(y),
                JPTarget::ImmediateWord(nn),
            ),
            (3, 2, _, 4, _) => Instruction::LD(
                // LD (0xFF00 + C) ,A
                LDTarget::Register(InstrRegisters::IndirectC),
                LDTarget::Register(InstrRegisters::A),
            ),
            (3, 2, _, 5, _) => Instruction::LD(
                // LD (nn), A
                LDTarget::ByteAtAddress(nn),
                LDTarget::Register(InstrRegisters::A),
            ),
            (3, 2, _, 6, _) => Instruction::LD(
                // LD A, (0xFF00 + C)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::Register(InstrRegisters::IndirectC),
            ),
            (3, 2, _, 7, _) => Instruction::LD(
                // LD A, (nn)
                LDTarget::Register(InstrRegisters::A),
                LDTarget::ByteAtAddress(nn),
            ),
            (3, 3, _, 0, _) => Instruction::JP(
                // JP nn
                JumpCondition::Always,
                JPTarget::ImmediateWord(nn),
            ),
            (3, 3, _, 1, _) => unreachable!("This is the 0xCB Prefix"),
            // (3, 3, _, 2, _) => unreachable!(), (removed in documentation)
            // (3, 3, _, 3, _) => unimplemented!(), (removed in documentation)
            // (3, 3, _, 4, _) => unimplemented!(), (removed in documentation)
            // (3, 3, _, 5, _) => unimplemented!(), (removed in documentation)
            (3, 3, _, 6, _) => Instruction::DI,
            (3, 3, _, 7, _) => Instruction::EI,
            (3, 4, _, 0..=3, _) => Instruction::CALL(Table::cc(y), nn), // CALL cc[y], nn
            // (3, 4, _, 4..=7, _) => unimplemented!(), (removed in documentation)
            (3, 5, 0, _, _) => Instruction::PUSH(Table::rp2(p)), // PUSH rp2[p]
            (3, 5, 1, _, 0) => Instruction::CALL(JumpCondition::Always, nn), // CALL nn
            // (3, 5, 1, _, 1..=3) => unimplemented!(), (removed in documentation)
            (3, 6, _, _, _) => Table::x3_alu(y, n),
            (3, 7, _, _, _) => Instruction::RST(y * 8), // RST y * 8
            _ => unimplemented!(
                "Unknown Opcode: {:#?}\n x: {}, z: {}, q: {}, y: {}, p: {}",
                opcode,
                x,
                z,
                q,
                y,
                p
            ),
        }
    }

    fn from_unprefixed_byte(cpu: &Cpu, byte: u8) -> Self {
        unimplemented!()
    }
}
pub enum JPTarget {
    RegisterPair(RegisterPair),
    ImmediateWord(u16),
}

pub enum AllRegisters {
    Byte(InstrRegisters),
    Word(RegisterPair),
}

pub enum MATHTarget {
    HL,
    SP,
    Register(InstrRegisters),
    RegisterPair(RegisterPair),
    ImmediateByte(u8),
}

pub enum LDTarget {
    Register(InstrRegisters),
    IndirectRegister(InstrRegisterPairs),
    ByteAtAddress(u16),
    ImmediateWord(u16),
    ImmediateByte(u8),
    RegisterPair(RegisterPair),
}

enum InstrRegisterPairs {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
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
    IndirectHL, // (HL)
    IndirectC,  // (0xFF00 + C)
}

pub enum JumpCondition {
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
            _ => unreachable!("Index {} is out of bounds in r[]", index),
        }
    }

    pub fn rp2(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::AF,
            _ => unreachable!("Index {} out of bounds in rp2[]", index),
        }
    }

    pub fn rp(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::SP,
            _ => unreachable!("Index {} out of bounds in rp[]", index),
        }
    }

    pub fn cc(index: u8) -> JumpCondition {
        match index {
            0 => JumpCondition::NotZero,
            1 => JumpCondition::Zero,
            2 => JumpCondition::NotCarry,
            3 => JumpCondition::Carry,
            _ => unreachable!("Index {} out of bounds in cc[]", index),
        }
    }

    pub fn x2_alu(fn_index: u8, r_index: u8) -> Instruction {
        match fn_index {
            0 => Instruction::ADD(
                // ADD A, r[z]
                MATHTarget::Register(InstrRegisters::A),
                MATHTarget::Register(Self::r(r_index)),
            ),
            1 => Instruction::ADC(
                // ADC A, r[z]
                MATHTarget::Register(InstrRegisters::A),
                MATHTarget::Register(Self::r(r_index)),
            ),
            2 => Instruction::SUB(MATHTarget::Register(Self::r(r_index))), // SUB r[z]
            3 => Instruction::SBC(
                // SBC A, r[z]
                MATHTarget::Register(InstrRegisters::A),
                MATHTarget::Register(Self::r(r_index)),
            ),
            4 => Instruction::AND(MATHTarget::Register(Self::r(r_index))), // AND r[z]
            5 => Instruction::XOR(MATHTarget::Register(Self::r(r_index))), // XOR r[z]
            6 => Instruction::OR(MATHTarget::Register(Self::r(r_index))),  // OR r[z]
            7 => Instruction::CP(MATHTarget::Register(Self::r(r_index))),  // CP r[z]
            _ => unreachable!("Index {} is out of bounds in alu[]"),
        }
    }

    pub fn x3_alu(fn_index: u8, byte: u8) -> Instruction {
        unimplemented!()
    }
}
