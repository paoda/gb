use super::cpu::{Cpu, Flags, Register, RegisterPair};
pub enum Instruction {
    NOP,
    LD(LDTarget, LDTarget),
    STOP,
    JR(JumpCondition, i8),
    ADD(MATHTarget, MATHTarget),
    INC(Registers),
    DEC(Registers),
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

pub struct Cycles(u8);

impl Instruction {
    pub fn execute(cpu: &mut Cpu, instruction: Self) -> Cycles {
        match instruction {
            Instruction::NOP => Cycles(4),
            Instruction::LD(lhs, rhs) => match (lhs, rhs) {
                (LDTarget::ByteAtAddress(nn), LDTarget::RegisterPair(RegisterPair::SP)) => {
                    // LD (nn), SP | Put Stack Pointer at address nn
                    cpu.write_word(nn, cpu.register_pair(RegisterPair::SP));
                    Cycles(20)
                }
                (LDTarget::RegisterPair(pairs), LDTarget::ImmediateWord(nn)) => {
                    // LD rp[p], nn | Put value nn into register pair
                    match pairs {
                        RegisterPair::BC => cpu.set_register_pair(RegisterPair::BC, nn),
                        RegisterPair::DE => cpu.set_register_pair(RegisterPair::DE, nn),
                        RegisterPair::HL => cpu.set_register_pair(RegisterPair::HL, nn),
                        RegisterPair::SP => cpu.set_register_pair(RegisterPair::SP, nn),
                        _ => unreachable!(),
                    }
                    Cycles(12)
                }
                (LDTarget::IndirectRegister(pair), LDTarget::Register(InstrRegister::A)) => {
                    let a = cpu.register(Register::A);

                    match pair {
                        InstrRegisterPair::BC => {
                            // LD (BC), A | Put A into memory address BC
                            let addr = cpu.register_pair(RegisterPair::BC);
                            cpu.write_byte(addr, a);
                        }
                        InstrRegisterPair::DE => {
                            // LD (DE), A | Put A into memory address DE
                            let addr = cpu.register_pair(RegisterPair::DE);
                            cpu.write_byte(addr, a);
                        }
                        InstrRegisterPair::IncrementHL => {
                            // LD (HL+), A | Put A into memory address HL, then increment HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, a);

                            cpu.set_register_pair(RegisterPair::HL, addr + 1);
                        }
                        InstrRegisterPair::DecrementHL => {
                            // LD (HL-), A | Put A into memory address HL, then decrement HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, a);

                            cpu.set_register_pair(RegisterPair::HL, addr - 1);
                        }
                        _ => unreachable!(),
                    }
                    Cycles(8)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::IndirectRegister(pair)) => {
                    match pair {
                        InstrRegisterPair::BC => {
                            // LD A, (BC) | Put value at address BC into A
                            let addr = cpu.register_pair(RegisterPair::BC);
                            cpu.set_register(Register::A, cpu.read_byte(addr));
                        }
                        InstrRegisterPair::DE => {
                            // LD A, (DE) | Put value at address DE into A
                            let addr = cpu.register_pair(RegisterPair::DE);
                            cpu.set_register(Register::A, cpu.read_byte(addr));
                        }
                        InstrRegisterPair::IncrementHL => {
                            // LD A, (HL+) | Put value at address HL into A, then increment HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.set_register(Register::A, cpu.read_byte(addr));

                            cpu.set_register_pair(RegisterPair::HL, addr + 1);
                        }
                        InstrRegisterPair::DecrementHL => {
                            // LD A, (HL-) | Put value at address HL into A, then increment HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.set_register(Register::A, cpu.read_byte(addr));

                            cpu.set_register_pair(RegisterPair::HL, addr - 1);
                        }
                        _ => unreachable!(),
                    }
                    Cycles(8)
                }
                (LDTarget::Register(reg), LDTarget::ImmediateByte(n)) => {
                    // LD r[y], n | Store n in Register
                    let cycles: Cycles;

                    match reg {
                        InstrRegister::B => {
                            cpu.set_register(Register::B, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::C => {
                            cpu.set_register(Register::C, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::D => {
                            cpu.set_register(Register::D, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::E => {
                            cpu.set_register(Register::E, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::H => {
                            cpu.set_register(Register::H, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::L => {
                            cpu.set_register(Register::L, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, n);
                            cycles = Cycles(12);
                        }
                        InstrRegister::A => {
                            cpu.set_register(Register::A, n);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }
                    cycles
                }
                _ => unimplemented!(),
            },
            Instruction::STOP => Cycles(4),
            Instruction::JR(cond, offset) => {
                // JR cc[y - 4], d | If condition is true, then add d to current address and jump
                // JR d | Add d to current address and jump
                let prev = cpu.register_pair(RegisterPair::PC);
                let flags: Flags = cpu.register(Register::Flag).into();
                let new_address = (prev as i16 + offset as i16) as u16;

                match cond {
                    JumpCondition::Always => {
                        cpu.set_register_pair(RegisterPair::PC, new_address);
                        Cycles(12)
                    }
                    JumpCondition::NotZero => {
                        if !flags.z {
                            cpu.set_register_pair(RegisterPair::PC, new_address);
                            return Cycles(12);
                        }

                        Cycles(8)
                    }
                    JumpCondition::Zero => {
                        if flags.z {
                            cpu.set_register_pair(RegisterPair::PC, new_address);
                            return Cycles(12);
                        }
                        Cycles(8)
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c {
                            cpu.set_register_pair(RegisterPair::PC, new_address);
                            return Cycles(12);
                        }
                        Cycles(8)
                    }
                    JumpCondition::Carry => {
                        if flags.c {
                            cpu.set_register_pair(RegisterPair::PC, new_address);
                            return Cycles(12);
                        }
                        Cycles(8)
                    }
                }
            }
            Instruction::ADD(lhs, rhs) => match (lhs, rhs) {
                (MATHTarget::RegisterPair(RegisterPair::HL), MATHTarget::RegisterPair(pair)) => {
                    // ADD HL, rp[p] | add register pair to HL.
                    let hl = cpu.register_pair(RegisterPair::HL);
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let sum;

                    match pair {
                        RegisterPair::BC => {
                            let bc = cpu.register_pair(RegisterPair::BC);
                            sum = Self::add_u16s(hl, bc, &mut flags);
                        }
                        RegisterPair::DE => {
                            let de = cpu.register_pair(RegisterPair::DE);
                            sum = Self::add_u16s(hl, de, &mut flags);
                        }
                        RegisterPair::HL => {
                            sum = Self::add_u16s(hl, hl, &mut flags);
                        }
                        RegisterPair::SP => {
                            let sp = cpu.register_pair(RegisterPair::SP);
                            sum = Self::add_u16s(hl, sp, &mut flags);
                        }
                        _ => unreachable!(),
                    }
                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register_pair(RegisterPair::HL, sum);
                    Cycles(8)
                }
                _ => unimplemented!(),
            },
            Instruction::INC(Registers::Word(pair)) => {
                // INC rp[p] | Increment Register Pair
                match pair {
                    RegisterPair::BC => {
                        let bc = cpu.register_pair(RegisterPair::BC);
                        cpu.set_register_pair(RegisterPair::BC, bc + 1);
                    }
                    RegisterPair::DE => {
                        let de = cpu.register_pair(RegisterPair::DE);
                        cpu.set_register_pair(RegisterPair::DE, de + 1);
                    }
                    RegisterPair::HL => {
                        let hl = cpu.register_pair(RegisterPair::HL);
                        cpu.set_register_pair(RegisterPair::HL, hl + 1);
                    }
                    RegisterPair::SP => {
                        let sp = cpu.register_pair(RegisterPair::SP);
                        cpu.set_register_pair(RegisterPair::SP, sp + 1);
                    }
                    _ => unreachable!(),
                }
                Cycles(8)
            }
            Instruction::INC(Registers::Byte(reg)) => {
                // INC r[y] | Increment Register
                let mut flags: Flags = cpu.register(Register::Flag).into();
                let cycles: Cycles;

                match reg {
                    InstrRegister::B => {
                        let b = cpu.register(Register::B);
                        cpu.set_register(Register::B, Self::inc_register(b, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::C => {
                        let c = cpu.register(Register::C);
                        cpu.set_register(Register::C, Self::inc_register(c, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::D => {
                        let d = cpu.register(Register::D);
                        cpu.set_register(Register::D, Self::inc_register(d, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::E => {
                        let e = cpu.register(Register::E);
                        cpu.set_register(Register::E, Self::inc_register(e, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::H => {
                        let h = cpu.register(Register::H);
                        cpu.set_register(Register::H, Self::inc_register(h, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::L => {
                        let l = cpu.register(Register::L);
                        cpu.set_register(Register::L, Self::inc_register(l, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        cpu.write_byte(addr, Self::inc_register(cpu.read_byte(addr), &mut flags));
                        cycles = Cycles(12);
                    }
                    InstrRegister::A => {
                        let a = cpu.register(Register::A);
                        cpu.set_register(Register::A, Self::inc_register(a, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::IndirectC => unreachable!(),
                }
                cpu.set_register(Register::Flag, flags.into());
                cycles
            }
            Instruction::DEC(Registers::Word(pair)) => {
                // DEC rp[p] | Decrement Register Pair
                match pair {
                    RegisterPair::BC => {
                        let bc = cpu.register_pair(RegisterPair::BC);
                        cpu.set_register_pair(RegisterPair::BC, bc - 1);
                    }
                    RegisterPair::DE => {
                        let de = cpu.register_pair(RegisterPair::DE);
                        cpu.set_register_pair(RegisterPair::DE, de - 1);
                    }
                    RegisterPair::HL => {
                        let hl = cpu.register_pair(RegisterPair::HL);
                        cpu.set_register_pair(RegisterPair::HL, hl - 1);
                    }
                    RegisterPair::SP => {
                        let sp = cpu.register_pair(RegisterPair::SP);
                        cpu.set_register_pair(RegisterPair::SP, sp - 1);
                    }
                    _ => unreachable!(),
                }
                Cycles(8)
            }
            Instruction::DEC(Registers::Byte(reg)) => {
                // DEC r[y] | Decrement Register
                let mut flags: Flags = cpu.register(Register::Flag).into();
                let cycles: Cycles;

                match reg {
                    InstrRegister::B => {
                        let b = cpu.register(Register::B);
                        cpu.set_register(Register::B, Self::dec_register(b, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::C => {
                        let c = cpu.register(Register::C);
                        cpu.set_register(Register::C, Self::dec_register(c, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::D => {
                        let d = cpu.register(Register::D);
                        cpu.set_register(Register::D, Self::dec_register(d, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::E => {
                        let e = cpu.register(Register::E);
                        cpu.set_register(Register::E, Self::dec_register(e, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::H => {
                        let h = cpu.register(Register::H);
                        cpu.set_register(Register::H, Self::dec_register(h, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::L => {
                        let l = cpu.register(Register::L);
                        cpu.set_register(Register::L, Self::dec_register(l, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        cpu.write_byte(addr, Self::dec_register(cpu.read_byte(addr), &mut flags));
                        cycles = Cycles(12);
                    }
                    InstrRegister::A => {
                        let a = cpu.register(Register::A);
                        cpu.set_register(Register::A, Self::dec_register(a, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::IndirectC => unreachable!(),
                }
                cpu.set_register(Register::Flag, flags.into());
                cycles
            }
            Instruction::RLCA => {
                let mut flags: Flags = cpu.register(Register::Flag).into();

                let a = cpu.register(Register::A);
                let cache = a >> 7; // get the 7th bit (this will be the carry bit + the one that is wrapped around)
                let rot_a = (a << 1) | (cache << 0); // (rotate a left), then set the first bit (which will be a 0 by default)

                flags.z = false;
                flags.n = false;
                flags.h = false;
                flags.c = cache == 0x01;

                cpu.set_register(Register::Flag, flags.into());
                cpu.set_register(Register::A, rot_a);
                Cycles(4)
            }
            Instruction::RRCA => {
                let mut flags: Flags = cpu.register(Register::Flag).into();

                let a = cpu.register(Register::A);
                let cache = a & 0x01; // RLCA but the other way around
                let rot_a = (a >> 1) | (cache << 7);

                flags.z = false;
                flags.n = false;
                flags.h = false;
                flags.c = cache == 0x01;

                cpu.set_register(Register::Flag, flags.into());
                cpu.set_register(Register::A, rot_a);
                Cycles(4)
            }
            Instruction::RLA => {
                let mut flags: Flags = cpu.register(Register::Flag).into();

                let a = cpu.register(Register::A);
                let cache = a >> 7;
                let rot_a = (a << 1) | ((flags.c as u8) << 0);

                flags.z = false;
                flags.n = false;
                flags.h = false;
                flags.c = cache == 0x01;

                cpu.set_register(Register::Flag, flags.into());
                cpu.set_register(Register::A, rot_a);
                Cycles(4)
            }
            Instruction::RRA => {
                let mut flags: Flags = cpu.register(Register::Flag).into();

                let a = cpu.register(Register::A);
                let cache = a & 0x01;
                let rot_a = (a >> 1) | ((flags.c as u8) << 7);

                flags.z = false;
                flags.n = false;
                flags.h = false;
                flags.c = cache == 0x01;

                cpu.set_register(Register::Flag, flags.into());
                cpu.set_register(Register::A, rot_a);
                Cycles(4)
            }
            Instruction::DAA => unimplemented!(),
            Instruction::CPL => {
                let mut flags: Flags = cpu.register(Register::Flag).into();
                let a = cpu.register(Register::A);

                flags.n = true;
                flags.h = true;

                cpu.set_register(Register::Flag, flags.into());
                cpu.set_register(Register::A, !a); // Bitwise not is ! instead of ~
                Cycles(4)
            }
            Instruction::SCF => {
                let mut flags: Flags = cpu.register(Register::Flag).into();

                flags.n = false;
                flags.h = false;
                flags.c = true;

                cpu.set_register(Register::Flag, flags.into());
                Cycles(4)
            }
            Instruction::CCF => {
                let mut flags: Flags = cpu.register(Register::Flag).into();

                flags.n = false;
                flags.h = false;
                flags.c = !flags.c;

                cpu.set_register(Register::Flag, flags.into());
                Cycles(4)
            }

            _ => unimplemented!(),
        }
    }

    fn dec_register(reg: u8, flags: &mut Flags) -> u8 {
        let res = reg - 1;

        flags.z = res == 0;
        flags.n = true;
        flags.h = !Self::u8_half_carry(res, 1); // FIXME: Is this right?

        res
    }

    fn inc_register(reg: u8, flags: &mut Flags) -> u8 {
        let res = reg + 1;

        flags.z = res == 0;
        flags.n = false;
        flags.h = Self::u8_half_carry(reg, 1);

        res
    }

    fn add_u16s(left: u16, right: u16, flags: &mut Flags) -> u16 {
        let (sum, did_overflow) = left.overflowing_add(right);
        flags.n = false;
        flags.h = Self::u16_half_carry(left, right);
        flags.c = did_overflow;

        sum
    }

    fn u16_half_carry(left: u16, right: u16) -> bool {
        Self::u8_half_carry((left >> 8) as u8, (right >> 8) as u8)
    }

    fn u8_half_carry(left: u8, right: u8) -> bool {
        ((left & 0xF) + (right & 0xF)) & 0x10 == 0x10
    }
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
                LDTarget::IndirectRegister(InstrRegisterPair::BC),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 0, _, 1) => Instruction::LD(
                // LD (DE), A
                LDTarget::IndirectRegister(InstrRegisterPair::DE),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 1, _, 0) => Instruction::LD(
                // LD A, (BC)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::BC),
            ),
            (0, 2, 1, _, 1) => Instruction::LD(
                // LD A, (DE)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::DE),
            ),
            (0, 2, 0, _, 2) => Instruction::LD(
                // LD (HL+), A
                LDTarget::IndirectRegister(InstrRegisterPair::IncrementHL),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 0, _, 3) => Instruction::LD(
                // LD (HL-), A
                LDTarget::IndirectRegister(InstrRegisterPair::DecrementHL),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 1, _, 2) => Instruction::LD(
                // LD A, (HL+)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::IncrementHL),
            ),
            (0, 2, 1, _, 3) => Instruction::LD(
                // LD A, (HL-)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::DecrementHL),
            ),
            (0, 3, 0, _, _) => Instruction::INC(
                // INC rp[p]
                Registers::Word(Table::rp(p)),
            ),
            (0, 3, 1, _, _) => Instruction::DEC(
                // DEC rp[p]
                Registers::Word(Table::rp(p)),
            ),
            (0, 4, _, _, _) => Instruction::INC(
                // INC r[y]
                Registers::Byte(Table::r(y)),
            ),
            (0, 5, _, _, _) => Instruction::DEC(
                // DEC r[y]
                Registers::Byte(Table::r(y)),
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
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 0, _, 5, _) => Instruction::ADD(
                // ADD SP, d
                MATHTarget::RegisterPair(RegisterPair::SP),
                MATHTarget::ImmediateByte(n),
            ),
            (3, 0, _, 6, _) => Instruction::LD(
                // LD A, (0xFF00 + n)
                LDTarget::Register(InstrRegister::A),
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
                LDTarget::Register(InstrRegister::IndirectC),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 2, _, 5, _) => Instruction::LD(
                // LD (nn), A
                LDTarget::ByteAtAddress(nn),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 2, _, 6, _) => Instruction::LD(
                // LD A, (0xFF00 + C)
                LDTarget::Register(InstrRegister::A),
                LDTarget::Register(InstrRegister::IndirectC),
            ),
            (3, 2, _, 7, _) => Instruction::LD(
                // LD A, (nn)
                LDTarget::Register(InstrRegister::A),
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

pub enum Registers {
    Byte(InstrRegister),
    Word(RegisterPair),
}

pub enum MATHTarget {
    HL,
    SP,
    Register(InstrRegister),
    RegisterPair(RegisterPair),
    ImmediateByte(u8),
}

pub enum LDTarget {
    Register(InstrRegister),
    IndirectRegister(InstrRegisterPair),
    ByteAtAddress(u16),
    ImmediateWord(u16),
    ImmediateByte(u8),
    RegisterPair(RegisterPair),
}

enum InstrRegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
    IncrementHL,
    DecrementHL,
}

enum InstrRegister {
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
    pub fn r(index: u8) -> InstrRegister {
        match index {
            0 => InstrRegister::B,
            1 => InstrRegister::C,
            2 => InstrRegister::D,
            3 => InstrRegister::E,
            4 => InstrRegister::H,
            5 => InstrRegister::L,
            6 => InstrRegister::IndirectHL,
            7 => InstrRegister::A,
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
                MATHTarget::Register(InstrRegister::A),
                MATHTarget::Register(Self::r(r_index)),
            ),
            1 => Instruction::ADC(
                // ADC A, r[z]
                MATHTarget::Register(InstrRegister::A),
                MATHTarget::Register(Self::r(r_index)),
            ),
            2 => Instruction::SUB(MATHTarget::Register(Self::r(r_index))), // SUB r[z]
            3 => Instruction::SBC(
                // SBC A, r[z]
                MATHTarget::Register(InstrRegister::A),
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
