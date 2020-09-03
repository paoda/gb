use super::cpu::{Cpu, Flags, Register, RegisterPair};
use std::convert::TryFrom;
#[derive(Debug, Copy, Clone)]
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

#[derive(Debug, Copy, Clone)]
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
                (LDTarget::RegisterPair(pair), LDTarget::ImmediateWord(nn)) => {
                    // LD rp[p], nn | Put value nn into register pair
                    match pair {
                        RegisterPair::BC
                        | RegisterPair::DE
                        | RegisterPair::HL
                        | RegisterPair::SP => {
                            cpu.set_register_pair(RegisterPair::try_from(pair).unwrap(), nn)
                        }
                        _ => unreachable!(),
                    }
                    Cycles(12)
                }
                (LDTarget::IndirectRegister(pair), LDTarget::Register(InstrRegister::A)) => {
                    let a = cpu.register(Register::A);
                    match pair {
                        InstrRegisterPair::BC | InstrRegisterPair::DE => {
                            // LD (BC), A | Put A into memory address BC
                            // LD (DE), A | Put A into memory address DE
                            let addr = cpu.register_pair(RegisterPair::try_from(pair).unwrap());
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
                        InstrRegisterPair::BC | InstrRegisterPair::DE => {
                            // LD A, (BC) | Put value at address BC into A
                            // LD A, (DE) | Put value at address DE into A
                            let addr = cpu.register_pair(RegisterPair::try_from(pair).unwrap());
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
                    match reg {
                        InstrRegister::IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, n);
                            Cycles(12)
                        }
                        InstrRegister::A
                        | InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L => {
                            cpu.set_register(Register::try_from(reg).unwrap(), n);
                            Cycles(8)
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }
                }
                (LDTarget::Register(lhs), LDTarget::Register(rhs)) => {
                    // LD r[y], r[z] | Store value of RHS Register in LHS Register

                    // FIXME: panicking is the right thing to do, but maybe .unwrap is too unclear?
                    let rhs_value = cpu.register(Register::try_from(rhs).unwrap());
                    cpu.set_register(Register::try_from(lhs).unwrap(), rhs_value);
                    Cycles(4)
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
                    let mut flags: Flags = cpu.register(Register::Flag).into();

                    match pair {
                        RegisterPair::BC
                        | RegisterPair::DE
                        | RegisterPair::HL
                        | RegisterPair::SP => {
                            let hl_value = cpu.register_pair(RegisterPair::HL);
                            let value = cpu.register_pair(RegisterPair::try_from(pair).unwrap());
                            let sum = Self::add_u16s(hl_value, value, &mut flags);

                            cpu.set_register_pair(RegisterPair::HL, sum);
                        }
                        _ => unreachable!(),
                    }
                    cpu.set_register(Register::Flag, flags.into());
                    Cycles(8)
                }
                (MATHTarget::Register(InstrRegister::A), MATHTarget::Register(reg)) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let sum;
                    let cycles: Cycles;
                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value = cpu.register(Register::try_from(reg).unwrap());
                            sum = Self::add_u8s(a_value, value, &mut flags);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            sum = Self::add_u8s(a_value, value, &mut flags);
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectC => unimplemented!(),
                    }

                    cpu.set_register(Register::A, sum);
                    cpu.set_register(Register::Flag, flags.into());
                    cycles
                }
                _ => unimplemented!(),
            },
            Instruction::INC(Registers::Word(pair)) => {
                // INC rp[p] | Increment Register Pair
                match pair {
                    RegisterPair::BC | RegisterPair::DE | RegisterPair::HL | RegisterPair::SP => {
                        let value = cpu.register_pair(pair);
                        cpu.set_register_pair(pair, value + 1);
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
                    InstrRegister::B
                    | InstrRegister::C
                    | InstrRegister::D
                    | InstrRegister::E
                    | InstrRegister::H
                    | InstrRegister::L
                    | InstrRegister::A => {
                        let reg = Register::try_from(reg).unwrap();

                        let value = cpu.register(reg);
                        cpu.set_register(reg, Self::inc_register(value, &mut flags));
                        cycles = Cycles(4)
                    }
                    InstrRegister::IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        cpu.write_byte(addr, Self::inc_register(cpu.read_byte(addr), &mut flags));
                        cycles = Cycles(12)
                    }
                    InstrRegister::IndirectC => unreachable!(),
                }
                cpu.set_register(Register::Flag, flags.into());
                cycles
            }
            Instruction::DEC(Registers::Word(pair)) => {
                match pair {
                    RegisterPair::BC | RegisterPair::DE | RegisterPair::HL | RegisterPair::SP => {
                        let value = cpu.register_pair(pair);
                        cpu.set_register_pair(pair, value - 1);
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
                    InstrRegister::B
                    | InstrRegister::C
                    | InstrRegister::D
                    | InstrRegister::E
                    | InstrRegister::H
                    | InstrRegister::L
                    | InstrRegister::A => {
                        let reg = Register::try_from(reg).unwrap();

                        let value = cpu.register(reg);
                        cpu.set_register(reg, Self::dec_register(value, &mut flags));
                        cycles = Cycles(4);
                    }
                    InstrRegister::IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        cpu.write_byte(addr, Self::dec_register(cpu.read_byte(addr), &mut flags));
                        cycles = Cycles(12);
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
            Instruction::HALT => unimplemented!(),
            Instruction::ADC(lhs, rhs) => match (lhs, rhs) {
                (MATHTarget::Register(InstrRegister::A), MATHTarget::Register(reg)) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;
                    let sum;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value =
                                cpu.register(Register::try_from(reg).unwrap()) + (flags.c as u8);
                            sum = Self::add_u8s(a_value, value, &mut flags);
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            sum = Self::add_u8s(a_value, value, &mut flags);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }
                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, sum);
                    cycles
                }
                _ => unimplemented!(),
            },
            Instruction::SUB(target) => match target {
                MATHTarget::Register(reg) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;
                    let diff;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value = cpu.register(Register::try_from(reg).unwrap());
                            diff = Self::sub_u8s(a_value, value, &mut flags);
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            diff = Self::sub_u8s(a_value, value, &mut flags);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, diff);
                    cycles
                }
                MATHTarget::ImmediateByte(byte) => unimplemented!(),
                _ => unreachable!(),
            },
            Instruction::SBC(lhs, rhs) => match (lhs, rhs) {
                // TODO: Does SBC actually have anything other than the A register on the LHS?
                (MATHTarget::Register(InstrRegister::A), MATHTarget::Register(reg)) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;
                    let diff;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value =
                                cpu.register(Register::try_from(reg).unwrap()) + (flags.c as u8);
                            diff = Self::sub_u8s(a_value, value, &mut flags);
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            diff = Self::sub_u8s(a_value, value, &mut flags);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    cpu.set_register(Register::A, diff);
                    cpu.set_register(Register::Flag, flags.into());
                    cycles
                }
                _ => unimplemented!(),
            },
            Instruction::AND(target) => match target {
                MATHTarget::Register(reg) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;
                    let result;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value = cpu.register(Register::try_from(reg).unwrap());
                            result = a_value & value;
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            result = a_value & value;
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    flags.z = result == 0;
                    flags.n = false;
                    flags.h = true;
                    flags.c = false;

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, result);
                    cycles
                }
                MATHTarget::ImmediateByte(byte) => unimplemented!(),
                _ => unreachable!(),
            },
            Instruction::XOR(target) => match target {
                MATHTarget::Register(reg) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;
                    let result;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value = cpu.register(Register::try_from(reg).unwrap());
                            result = a_value ^ value;
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            result = a_value ^ value;
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    flags.z = result == 0;
                    flags.n = false;
                    flags.h = false;
                    flags.c = false;

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, result);
                    cycles
                }
                MATHTarget::ImmediateByte(byte) => unimplemented!(),
                _ => unreachable!(),
            },
            Instruction::OR(target) => match target {
                MATHTarget::Register(reg) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;
                    let result;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value = cpu.register(Register::try_from(reg).unwrap());
                            result = a_value | value;
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            result = a_value | value;
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    flags.z = result == 0;
                    flags.n = false;
                    flags.h = false;
                    flags.c = false;

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, result);
                    cycles
                }
                MATHTarget::ImmediateByte(byte) => unimplemented!(),
                _ => unreachable!(),
            },
            Instruction::CP(target) => match target {
                MATHTarget::Register(reg) => {
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let a_value = cpu.register(Register::A);
                    let cycles: Cycles;

                    match reg {
                        InstrRegister::B
                        | InstrRegister::C
                        | InstrRegister::D
                        | InstrRegister::E
                        | InstrRegister::H
                        | InstrRegister::L
                        | InstrRegister::A => {
                            let value = cpu.register(Register::try_from(reg).unwrap());
                            let _ = Self::sub_u8s(a_value, value, &mut flags);
                            cycles = Cycles(4);
                        }
                        InstrRegister::IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            let _ = Self::sub_u8s(a_value, value, &mut flags);
                            cycles = Cycles(8);
                        }
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    cpu.set_register(Register::Flag, flags.into());
                    cycles
                }
                MATHTarget::ImmediateByte(byte) => unimplemented!(),
                _ => unreachable!(),
            },
            _ => unimplemented!(),
        }
    }

    fn dec_register(reg: u8, flags: &mut Flags) -> u8 {
        Self::sub_u8s_no_carry(reg, 1, flags)
    }

    fn inc_register(reg: u8, flags: &mut Flags) -> u8 {
        Self::add_u8s_no_carry(reg, 1, flags)
    }

    fn sub_u8s_no_carry(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let diff = left.wrapping_sub(right);

        flags.z = diff == 0;
        flags.n = true;
        flags.h = Self::u8_half_carry(left, right);

        diff
    }

    fn sub_u8s(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let (diff, did_overflow) = left.overflowing_sub(right);

        flags.z = diff == 0;
        flags.n = true;
        flags.h = Self::u8_half_carry(left, right);
        flags.c = did_overflow;

        diff
    }

    fn add_u8s_no_carry(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let sum = left.wrapping_add(right);

        flags.z = sum == 0;
        flags.n = false;
        flags.h = Self::u8_half_carry(left, right);

        sum
    }

    fn add_u8s(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let (sum, did_overflow) = left.overflowing_add(right);

        flags.z = sum == 0;
        flags.n = false;
        flags.h = Self::u8_half_carry(left, right);
        flags.c = did_overflow;

        sum
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

#[derive(Debug, Copy, Clone)]
pub enum JPTarget {
    RegisterPair(RegisterPair),
    ImmediateWord(u16),
}

#[derive(Debug, Copy, Clone)]
pub enum Registers {
    Byte(InstrRegister),
    Word(RegisterPair),
}

#[derive(Debug, Copy, Clone)]
pub enum MATHTarget {
    HL,
    SP,
    Register(InstrRegister),
    RegisterPair(RegisterPair),
    ImmediateByte(u8),
}

#[derive(Debug, Copy, Clone)]
pub enum LDTarget {
    Register(InstrRegister),
    IndirectRegister(InstrRegisterPair),
    ByteAtAddress(u16),
    ImmediateWord(u16),
    ImmediateByte(u8),
    RegisterPair(RegisterPair),
}

#[derive(Debug, Copy, Clone)]
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

impl From<RegisterPair> for InstrRegisterPair {
    fn from(pair: RegisterPair) -> Self {
        match pair {
            RegisterPair::AF => Self::AF,
            RegisterPair::BC => Self::BC,
            RegisterPair::DE => Self::DE,
            RegisterPair::HL => Self::HL,
            RegisterPair::SP => Self::SP,
            RegisterPair::PC => Self::PC,
        }
    }
}

impl TryFrom<InstrRegisterPair> for RegisterPair {
    type Error = String; // FIXME: Proper error type goes here.

    fn try_from(pair: InstrRegisterPair) -> Result<Self, Self::Error> {
        match pair {
            InstrRegisterPair::AF => Ok(Self::AF),
            InstrRegisterPair::BC => Ok(Self::BC),
            InstrRegisterPair::DE => Ok(Self::DE),
            InstrRegisterPair::HL => Ok(Self::HL),
            InstrRegisterPair::SP => Ok(Self::SP),
            InstrRegisterPair::PC => Ok(Self::PC),
            InstrRegisterPair::IncrementHL => {
                Err("Can not convert InstrRegisterPair::IncrementHL to RegisterPair".to_string())
            }
            InstrRegisterPair::DecrementHL => {
                Err("Can not convert InstrRegisterPair::DecrementHL to RegisterPair".to_string())
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
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

impl TryFrom<Register> for InstrRegister {
    type Error = String; // FIXME: Proper error type goes here

    fn try_from(register: Register) -> Result<Self, Self::Error> {
        match register {
            Register::A => Ok(Self::A),
            Register::B => Ok(Self::B),
            Register::C => Ok(Self::C),
            Register::D => Ok(Self::D),
            Register::E => Ok(Self::E),
            Register::H => Ok(Self::H),
            Register::L => Ok(Self::L),
            Register::Flag => Err("Can not convert Register::Flag to InstrRegister".to_string()),
        }
    }
}

impl TryFrom<InstrRegister> for Register {
    type Error = String; // FIXME: Proper error type goes here.

    fn try_from(register: InstrRegister) -> Result<Self, Self::Error> {
        match register {
            InstrRegister::A => Ok(Self::A),
            InstrRegister::B => Ok(Self::B),
            InstrRegister::C => Ok(Self::C),
            InstrRegister::D => Ok(Self::D),
            InstrRegister::E => Ok(Self::E),
            InstrRegister::H => Ok(Self::H),
            InstrRegister::L => Ok(Self::L),
            InstrRegister::IndirectHL => {
                Err("Can not convert InstrRegister::IndirectHL to Register".to_string())
            }
            InstrRegister::IndirectC => {
                Err("Can not convert InstrRegister::IndirectC to Register".to_string())
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

#[derive(Debug, Copy, Clone)]
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
