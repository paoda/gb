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
    ADC(MATHTarget), // ADC A, MATHTarget
    SUB(MATHTarget), // SUB A, MATHTarget
    SBC(MATHTarget),
    AND(MATHTarget), // AND A, MATHTarget
    XOR(MATHTarget), // XOR A, MATHTarget
    OR(MATHTarget),  // OR A, MATHTarget
    CP(MATHTarget),  // CP A, MATHTarget
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
    RLC(InstrRegister),
    RRC(InstrRegister),
    RL(InstrRegister),
    RR(InstrRegister),
    SLA(InstrRegister),
    SRA(InstrRegister),
    SWAP(InstrRegister),
    SRL(InstrRegister),
    BIT(u8, InstrRegister),
    RES(u8, InstrRegister),
    SET(u8, InstrRegister),
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
                (
                    LDTarget::Register(InstrRegister::IndirectC),
                    LDTarget::Register(InstrRegister::A),
                ) => {
                    // LD (0xFF00 + C), A | Store value of register A at address 0xFF00 + C
                    let addr = 0xFF00 + cpu.register(Register::C) as u16;
                    cpu.write_byte(addr, cpu.register(Register::A));
                    Cycles(8)
                }
                (
                    LDTarget::Register(InstrRegister::A),
                    LDTarget::Register(InstrRegister::IndirectC),
                ) => {
                    let addr = 0xFF00 + cpu.register(Register::C) as u16;
                    cpu.set_register(Register::A, cpu.read_byte(addr));
                    Cycles(8)
                }
                (LDTarget::Register(lhs), LDTarget::Register(rhs)) => {
                    // LD r[y], r[z] | Store value of RHS Register in LHS Register

                    // FIXME: panicking is the right thing to do, but maybe .unwrap is too unclear?
                    let rhs_value = cpu.register(Register::try_from(rhs).unwrap());
                    cpu.set_register(Register::try_from(lhs).unwrap(), rhs_value);
                    Cycles(4)
                }
                (LDTarget::ByteAtAddressWithOffset(n), LDTarget::Register(InstrRegister::A)) => {
                    // LD (0xFF00 + n), A | Store register A at address (0xFF00 + n)
                    cpu.write_byte(0xFF00 + (n as u16), cpu.register(Register::A));
                    Cycles(12)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::ByteAtAddressWithOffset(n)) => {
                    // LD A, (0xFF00 + n) | Store value at address (0xFF00 + n) in register A
                    cpu.set_register(Register::A, cpu.read_byte(0xFF00 + (n as u16)));
                    Cycles(12)
                }
                (
                    LDTarget::RegisterPair(RegisterPair::SP),
                    LDTarget::RegisterPair(RegisterPair::HL),
                ) => {
                    // LD SP, HL | Load Register HL into Register SP
                    cpu.set_register_pair(RegisterPair::SP, cpu.register_pair(RegisterPair::HL));
                    Cycles(8)
                }
                (LDTarget::ByteAtAddress(nn), LDTarget::Register(InstrRegister::A)) => {
                    cpu.write_byte(nn, cpu.register(Register::A));
                    Cycles(16)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::ByteAtAddress(nn)) => {
                    cpu.set_register(Register::A, cpu.read_byte(nn));
                    Cycles(16)
                }
                _ => unimplemented!(),
            },
            Instruction::STOP => Cycles(4),
            Instruction::JR(cond, offset) => {
                // JR cc[y - 4], d | If condition is true, then add d to current address and jump
                // JR d | Add d to current address and jump
                let prev = cpu.register_pair(RegisterPair::PC);
                let flags: Flags = cpu.register(Register::Flag).into();
                let new_address = Self::add_u16_i8_no_flags(prev, offset);

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
                    // ADD A, r[z] | Add (A + r[z]) to register A
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
                        InstrRegister::IndirectC => unreachable!(),
                    }

                    cpu.set_register(Register::A, sum);
                    cpu.set_register(Register::Flag, flags.into());
                    cycles
                }
                (MATHTarget::RegisterPair(RegisterPair::SP), MATHTarget::ImmediateByte(d)) => {
                    // ADD SP, d | Add d (is signed) to register pair SP.
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let d = d as i8;
                    let sum = Self::add_u16_i8(cpu.register_pair(RegisterPair::SP), d, &mut flags);
                    cpu.set_register_pair(RegisterPair::SP, sum);
                    Cycles(16)
                }
                (MATHTarget::Register(InstrRegister::A), MATHTarget::ImmediateByte(n)) => {
                    // ADD A, n | Add n to register A
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let sum = Self::add_u8s(cpu.register(Register::A), n, &mut flags);

                    cpu.set_register(Register::A, sum);
                    cpu.set_register(Register::Flag, flags.into());
                    Cycles(8)
                }
                _ => unreachable!(),
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
                // DEC rp[p] | Decrement Register Pair
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
                // Rotate Register A left
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
                // Rotate Register A right
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
                // Rotate register A left through carry
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
                // Rotate register A right through carry
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
                // Compliment A register (inverse)
                let mut flags: Flags = cpu.register(Register::Flag).into();
                let a = cpu.register(Register::A);

                flags.n = true;
                flags.h = true;

                cpu.set_register(Register::Flag, flags.into());
                cpu.set_register(Register::A, !a); // Bitwise not is ! instead of ~
                Cycles(4)
            }
            Instruction::SCF => {
                // Set Carry Flag
                let mut flags: Flags = cpu.register(Register::Flag).into();

                flags.n = false;
                flags.h = false;
                flags.c = true;

                cpu.set_register(Register::Flag, flags.into());
                Cycles(4)
            }
            Instruction::CCF => {
                // Compliment Carry Flag (inverse)
                let mut flags: Flags = cpu.register(Register::Flag).into();

                flags.n = false;
                flags.h = false;
                flags.c = !flags.c;

                cpu.set_register(Register::Flag, flags.into());
                Cycles(4)
            }
            Instruction::HALT => unimplemented!(),
            Instruction::ADC(target) => match target {
                MATHTarget::Register(reg) => {
                    // ADC A, r[z] | Add register r[z] plus the Carry flag to A
                    // FIXME: Do I Add register A as well?
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
                MATHTarget::ImmediateByte(n) => {
                    // ADC A, n | Add immediate byte plus the carry flag to A
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let value = n + (flags.c as u8);
                    let sum = Self::add_u8s(cpu.register(Register::A), value, &mut flags);

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, sum);
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::SUB(target) => match target {
                MATHTarget::Register(reg) => {
                    // SUB r[z] | Subtract the value in register r[z] from register A, then store in A
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
                MATHTarget::ImmediateByte(n) => {
                    // SUB n | Subtract the immediate byte from register A, then store in A
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let diff = Self::sub_u8s(cpu.register(Register::A), n, &mut flags);

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, diff);
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::SBC(target) => match target {
                MATHTarget::Register(reg) => {
                    // SBC A, r[z] | Subtract the value from register r[z] from A, add the Carry flag and then store in A
                    // FIXME: See ADC, is this a correct understanding of this Instruction
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
                MATHTarget::ImmediateByte(n) => {
                    // SBC A, n | Subtract the value from immediate byte from A, add the carry flag and then store in A
                    // FIXME: The Fixme aboe applies to this variant as well
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let value = n + (flags.c as u8);
                    let diff = Self::sub_u8s(cpu.register(Register::A), value, &mut flags);

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, diff);
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::AND(target) => match target {
                MATHTarget::Register(reg) => {
                    // AND r[z] | Bitwise AND register r[z] and register A, store in register A
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
                MATHTarget::ImmediateByte(n) => {
                    // AND n | Bitwise AND immediate byte and register A, sotre in register A
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let result = cpu.register(Register::A) & n;

                    flags.z = result == 0;
                    flags.n = false;
                    flags.h = true;
                    flags.c = false;

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, result);
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::XOR(target) => match target {
                MATHTarget::Register(reg) => {
                    // XOR r[z] | Bitwise XOR register r[z] and register A, store in register A
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
                MATHTarget::ImmediateByte(n) => {
                    // XOR n | Bitwise XOR immediate byte and register A, store in register A
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let result = cpu.register(Register::A) ^ n;

                    flags.z = result == 0;
                    flags.n = false;
                    flags.h = false;
                    flags.c = false;

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, result);
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::OR(target) => match target {
                MATHTarget::Register(reg) => {
                    // OR r[z] | Bitwise OR register r[z] and register A, store in register A
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
                MATHTarget::ImmediateByte(n) => {
                    // OR n | Bitwise OR on immediate byte n and register A, store in register A
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let result = cpu.register(Register::A) | n;

                    flags.z = result == 0;
                    flags.n = false;
                    flags.h = false;
                    flags.c = false;

                    cpu.set_register(Register::Flag, flags.into());
                    cpu.set_register(Register::A, result);
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::CP(target) => match target {
                MATHTarget::Register(reg) => {
                    // CP r[z] | Same behaviour as SUB, except the result is not stored.
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
                MATHTarget::ImmediateByte(n) => {
                    // CP n | Same behaviour as SUB, except the result is not stored,
                    let mut flags: Flags = cpu.register(Register::Flag).into();
                    let _ = Self::sub_u8s(cpu.register(Register::A), n, &mut flags);

                    cpu.set_register(Register::Flag, flags.into());
                    Cycles(8)
                }
                _ => unreachable!(),
            },
            Instruction::RET(cond) => {
                // RET cc[y] | Essentially a POP PC, Return from Subroutine
                // RET       | Essentially a POP PC, Return from Subroutine
                let flags: Flags = cpu.register(Register::Flag).into();
                let sp_value = cpu.register_pair(RegisterPair::SP);

                match cond {
                    JumpCondition::NotZero => {
                        if !flags.z {
                            let (new_sp, addr) = Self::ret(cpu, sp_value);
                            cpu.set_register_pair(RegisterPair::SP, new_sp);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            return Cycles(20);
                        }
                        Cycles(8)
                    }
                    JumpCondition::Zero => {
                        if flags.z {
                            let (new_sp, addr) = Self::ret(cpu, sp_value);
                            cpu.set_register_pair(RegisterPair::SP, new_sp);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            return Cycles(20);
                        }
                        Cycles(8)
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c {
                            let (new_sp, addr) = Self::ret(cpu, sp_value);
                            cpu.set_register_pair(RegisterPair::SP, new_sp);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            return Cycles(20);
                        }
                        Cycles(8)
                    }
                    JumpCondition::Carry => {
                        if flags.c {
                            let (new_sp, addr) = Self::ret(cpu, sp_value);
                            cpu.set_register_pair(RegisterPair::SP, new_sp);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            return Cycles(20);
                        }
                        Cycles(8)
                    }
                    JumpCondition::Always => {
                        let (new_sp, addr) = Self::ret(cpu, sp_value);
                        cpu.set_register_pair(RegisterPair::SP, new_sp);
                        cpu.set_register_pair(RegisterPair::PC, addr);
                        Cycles(16)
                    }
                }
            }
            Instruction::LDHL(d) => {
                // LDHL SP + d   | Add SP + d to register HL
                // LD HL, SP + d | Add SP + d to register HL
                let mut flags: Flags = cpu.register(Register::Flag).into();
                let sum = Self::add_u16_i8(cpu.register_pair(RegisterPair::SP), d, &mut flags);
                cpu.set_register_pair(RegisterPair::HL, sum);
                Cycles(12)
            }
            Instruction::POP(pair) => {
                // POP rp2[p] | Pop from stack into register pair rp[2]
                // Flags are set when we call cpu.set_register_pair(RegisterPair::AF, value);
                let mut sp_value = cpu.register_pair(RegisterPair::SP);

                match pair {
                    RegisterPair::BC | RegisterPair::DE | RegisterPair::HL | RegisterPair::AF => {
                        let low = cpu.read_byte(sp_value);
                        sp_value += 1;
                        let high = cpu.read_byte(sp_value);
                        sp_value += 1;

                        cpu.set_register_pair(pair, (high as u16) << 8 | low as u16);
                    }
                    _ => unreachable!(),
                }
                cpu.set_register_pair(RegisterPair::SP, sp_value);
                Cycles(12)
            }
            Instruction::RETI => {
                // Same as RET, after which interrupts are enabled.
                let (new_sp, addr) = Self::ret(cpu, cpu.register_pair(RegisterPair::SP));
                cpu.set_register_pair(RegisterPair::SP, new_sp);
                cpu.set_register_pair(RegisterPair::PC, addr);
                cpu.set_ime(true);
                Cycles(16)
            }
            Instruction::JP(cond, target) => match target {
                JPTarget::RegisterPair(RegisterPair::HL) => {
                    // JP HL | Load register pair HL into program counter
                    cpu.set_register_pair(RegisterPair::PC, cpu.register_pair(RegisterPair::HL));
                    Cycles(4)
                }
                JPTarget::ImmediateWord(nn) => {
                    // JP cc[y], nn | Store Immediate Word in the Program Counter if cond is met
                    // JP nn        | Store Immediate Word in the Program Counter
                    let flags: Flags = cpu.register(Register::Flag).into();

                    match cond {
                        JumpCondition::NotZero => {
                            if !flags.z {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                return Cycles(16);
                            }
                            Cycles(12)
                        }
                        JumpCondition::Zero => {
                            if flags.z {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                return Cycles(16);
                            }
                            Cycles(12)
                        }
                        JumpCondition::NotCarry => {
                            if !flags.c {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                return Cycles(16);
                            }
                            Cycles(12)
                        }
                        JumpCondition::Carry => {
                            if flags.c {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                return Cycles(16);
                            }
                            Cycles(12)
                        }
                        JumpCondition::Always => {
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            Cycles(16)
                        }
                    }
                }
                _ => unreachable!(),
            },
            Instruction::DI => {
                // Disable IME
                cpu.set_ime(false);
                Cycles(4)
            }
            Instruction::EI => {
                // Enable IME (After the next instruction)
                // FIXME: IME is set after the next instruction, this currently is not represented in this emulator.
                cpu.set_ime(true);
                Cycles(4)
            }
            Instruction::CALL(cond, nn) => {
                // CALL cc[y], nn | Store nn on the stack, then store nn in the program coutner if cond is met
                // CALL nn        | Store nn on the stack, then store nn in the program counter
                let flags: Flags = cpu.register(Register::Flag).into();
                let mut sp_value = cpu.register_pair(RegisterPair::SP);

                // TODO: I repeat myself a lot here.
                match cond {
                    JumpCondition::NotZero => {
                        if !flags.z {
                            sp_value -= 1;
                            cpu.write_byte(sp_value, (nn >> 8) as u8);
                            sp_value -= 1;
                            cpu.write_byte(sp_value, nn as u8);

                            cpu.set_register_pair(RegisterPair::SP, sp_value);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            return Cycles(24);
                        }
                        Cycles(12)
                    }
                    JumpCondition::Zero => {
                        if flags.z {
                            sp_value -= 1;
                            cpu.write_byte(sp_value, (nn >> 8) as u8);
                            sp_value -= 1;
                            cpu.write_byte(sp_value, nn as u8);

                            cpu.set_register_pair(RegisterPair::SP, sp_value);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            return Cycles(24);
                        }
                        Cycles(12)
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c {
                            sp_value -= 1;
                            cpu.write_byte(sp_value, (nn >> 8) as u8);
                            sp_value -= 1;
                            cpu.write_byte(sp_value, nn as u8);

                            cpu.set_register_pair(RegisterPair::SP, sp_value);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            return Cycles(24);
                        }
                        Cycles(12)
                    }
                    JumpCondition::Carry => {
                        if flags.c {
                            sp_value -= 1;
                            cpu.write_byte(sp_value, (nn >> 8) as u8);
                            sp_value -= 1;
                            cpu.write_byte(sp_value, nn as u8);

                            cpu.set_register_pair(RegisterPair::SP, sp_value);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            return Cycles(24);
                        }
                        Cycles(12)
                    }
                    JumpCondition::Always => {
                        sp_value -= 1;
                        cpu.write_byte(sp_value, (nn >> 8) as u8);
                        sp_value -= 1;
                        cpu.write_byte(sp_value, nn as u8);

                        cpu.set_register_pair(RegisterPair::SP, sp_value);
                        cpu.set_register_pair(RegisterPair::PC, nn);
                        Cycles(24)
                    }
                }
            }
            Instruction::PUSH(pair) => {
                // PUSH rp2[p] | Push register pair onto the stack
                // Note: Flags are mutated in this instruction if the pair is RegisterPair::AF
                let mut sp_value = cpu.register_pair(RegisterPair::SP);

                match pair {
                    RegisterPair::BC | RegisterPair::DE | RegisterPair::HL | RegisterPair::AF => {
                        let value = cpu.register_pair(pair);

                        sp_value -= 1;
                        cpu.write_byte(sp_value, (value >> 8) as u8);
                        sp_value -= 1;
                        cpu.write_byte(sp_value, value as u8);
                    }
                    _ => unreachable!(),
                }
                cpu.set_register_pair(RegisterPair::SP, sp_value);
                Cycles(16)
            }

            _ => unimplemented!(),
        }
    }

    /// POPs two bytes from the stack and concatenates them to form a u16 address
    ///
    /// Returns a Tuple containing the new stack pointer and the address from the stack
    /// * e.g. `(new_stack_pointer, address)`
    fn ret(cpu: &Cpu, sp: u16) -> (u16, u16) {
        let addr = (cpu.read_byte(sp + 1) as u16) << 8 | cpu.read_byte(sp) as u16;
        (sp + 2, addr)
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

    fn add_u16_i8_no_flags(left: u16, right: i8) -> u16 {
        (left as i16 + right as i16) as u16
    }

    fn add_u16_i8(left: u16, right: i8, flags: &mut Flags) -> u16 {
        let (sum, did_overflow) = left.overflowing_add(right as u16);

        flags.z = false;
        flags.n = false;
        flags.h = Self::u16_half_carry(left, right as u16);
        flags.c = did_overflow;

        sum
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
            Self::from_prefixed_byte(cpu)
        } else {
            Self::from_unprefixed_byte(cpu, byte)
        }
    }

    fn from_unprefixed_byte(cpu: &Cpu, opcode: u8) -> Self {
        // https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        let x = (opcode >> 6) & 0b00000011;
        let y = (opcode >> 3) & 0b00000111;
        let z = opcode & 0b00000111;
        let p = y >> 1;
        let q = y & 0b00000001;

        let n = cpu.read_byte(cpu.register_pair(RegisterPair::PC) + 1);
        let nn = cpu.read_word(cpu.register_pair(RegisterPair::PC) + 1);

        match (x, z, q, y, p) {
            (0, 0, _, 0, _) => Self::NOP, // NOP
            (0, 0, _, 1, _) => Self::LD(
                // LD (nn), SP
                LDTarget::ByteAtAddress(nn),
                LDTarget::RegisterPair(RegisterPair::SP),
            ),
            (0, 0, _, 2, _) => Self::STOP, // STOP
            (0, 0, _, 3, _) => Self::JR(JumpCondition::Always, n as i8), // JR d
            (0, 0, _, 4..=7, _) => Self::JR(Table::cc(y - 4), n as i8), // JR cc[y - 4], d
            (0, 1, 0, _, _) => Self::LD(
                // LD rp[p], nn
                LDTarget::RegisterPair(Table::rp(p)),
                LDTarget::ImmediateWord(nn),
            ),
            (0, 1, 1, _, _) => Self::ADD(
                // ADD HL, rp[p]
                MATHTarget::HL,
                MATHTarget::RegisterPair(Table::rp(p)),
            ),
            (0, 2, 0, _, 0) => Self::LD(
                // LD (BC), A
                LDTarget::IndirectRegister(InstrRegisterPair::BC),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 0, _, 1) => Self::LD(
                // LD (DE), A
                LDTarget::IndirectRegister(InstrRegisterPair::DE),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 1, _, 0) => Self::LD(
                // LD A, (BC)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::BC),
            ),
            (0, 2, 1, _, 1) => Self::LD(
                // LD A, (DE)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::DE),
            ),
            (0, 2, 0, _, 2) => Self::LD(
                // LD (HL+), A
                LDTarget::IndirectRegister(InstrRegisterPair::IncrementHL),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 0, _, 3) => Self::LD(
                // LD (HL-), A
                LDTarget::IndirectRegister(InstrRegisterPair::DecrementHL),
                LDTarget::Register(InstrRegister::A),
            ),
            (0, 2, 1, _, 2) => Self::LD(
                // LD A, (HL+)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::IncrementHL),
            ),
            (0, 2, 1, _, 3) => Self::LD(
                // LD A, (HL-)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectRegister(InstrRegisterPair::DecrementHL),
            ),
            (0, 3, 0, _, _) => Self::INC(
                // INC rp[p]
                Registers::Word(Table::rp(p)),
            ),
            (0, 3, 1, _, _) => Self::DEC(
                // DEC rp[p]
                Registers::Word(Table::rp(p)),
            ),
            (0, 4, _, _, _) => Self::INC(
                // INC r[y]
                Registers::Byte(Table::r(y)),
            ),
            (0, 5, _, _, _) => Self::DEC(
                // DEC r[y]
                Registers::Byte(Table::r(y)),
            ),
            (0, 6, _, _, _) => Self::LD(
                // LD r[y], n
                LDTarget::Register(Table::r(y)),
                LDTarget::ImmediateByte(n),
            ),
            (0, 7, _, 0, _) => Self::RLCA,
            (0, 7, _, 1, _) => Self::RRCA,
            (0, 7, _, 2, _) => Self::RLA,
            (0, 7, _, 3, _) => Self::RRA,
            (0, 7, _, 4, _) => Self::DAA,
            (0, 7, _, 5, _) => Self::CPL,
            (0, 7, _, 6, _) => Self::SCF,
            (0, 7, _, 7, _) => Self::CCF,
            (1, 6, _, 6, _) => Self::HALT,
            (1, _, _, _, _) => Self::LD(
                // LD r[y], r[z]
                LDTarget::Register(Table::r(y)),
                LDTarget::Register(Table::r(z)),
            ),
            (2, _, _, _, _) => Table::x2_alu(y, z), // alu[y] r[z]
            (3, 0, _, 0..=3, _) => Self::RET(Table::cc(y)), // RET cc[y]
            (3, 0, _, 4, _) => Self::LD(
                // LD (0xFF00 + n), A
                LDTarget::ByteAtAddressWithOffset(n),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 0, _, 5, _) => Self::ADD(
                // ADD SP, d
                MATHTarget::RegisterPair(RegisterPair::SP),
                MATHTarget::ImmediateByte(n),
            ),
            (3, 0, _, 6, _) => Self::LD(
                // LD A, (0xFF00 + n)
                LDTarget::Register(InstrRegister::A),
                LDTarget::ByteAtAddressWithOffset(n),
            ),
            (3, 0, _, 7, _) => Self::LDHL(n as i8), // LD HL, SP + d
            (3, 1, 0, _, _) => Self::POP(Table::rp2(p)), // POP rp2[p]
            (3, 1, 1, _, 0) => Self::RET(JumpCondition::Always), // RET
            (3, 1, 1, _, 1) => Self::RETI,
            (3, 1, 1, _, 2) => Self::JP(
                // JP HL
                JumpCondition::Always,
                JPTarget::RegisterPair(RegisterPair::HL),
            ),
            (3, 1, 1, _, 3) => Self::LD(
                // LD SP, HL
                LDTarget::RegisterPair(RegisterPair::SP),
                LDTarget::RegisterPair(RegisterPair::HL),
            ),
            (3, 2, _, 0..=3, _) => Self::JP(
                // JP cc[y], nn
                Table::cc(y),
                JPTarget::ImmediateWord(nn),
            ),
            (3, 2, _, 4, _) => Self::LD(
                // LD (0xFF00 + C) ,A
                LDTarget::Register(InstrRegister::IndirectC),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 2, _, 5, _) => Self::LD(
                // LD (nn), A
                LDTarget::ByteAtAddress(nn),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 2, _, 6, _) => Self::LD(
                // LD A, (0xFF00 + C)
                LDTarget::Register(InstrRegister::A),
                LDTarget::Register(InstrRegister::IndirectC),
            ),
            (3, 2, _, 7, _) => Self::LD(
                // LD A, (nn)
                LDTarget::Register(InstrRegister::A),
                LDTarget::ByteAtAddress(nn),
            ),
            (3, 3, _, 0, _) => Self::JP(
                // JP nn
                JumpCondition::Always,
                JPTarget::ImmediateWord(nn),
            ),
            (3, 3, _, 1, _) => unreachable!("This is the 0xCB Prefix"),
            // (3, 3, _, 2, _) => unreachable!(), (removed in documentation)
            // (3, 3, _, 3, _) => unreachable!(), (removed in documentation)
            // (3, 3, _, 4, _) => unreachable!(), (removed in documentation)
            // (3, 3, _, 5, _) => unreachable!(), (removed in documentation)
            (3, 3, _, 6, _) => Self::DI,
            (3, 3, _, 7, _) => Self::EI,
            (3, 4, _, 0..=3, _) => Self::CALL(Table::cc(y), nn), // CALL cc[y], nn
            // (3, 4, _, 4..=7, _) => unreachable!(), (removed in documentation)
            (3, 5, 0, _, _) => Self::PUSH(Table::rp2(p)), // PUSH rp2[p]
            (3, 5, 1, _, 0) => Self::CALL(JumpCondition::Always, nn), // CALL nn
            // (3, 5, 1, _, 1..=3) => unreachable!(), (removed in documentation)
            (3, 6, _, _, _) => Table::x3_alu(y, n),
            (3, 7, _, _, _) => Self::RST(y * 8), // RST y * 8
            _ => panic!(
                "Unknown Opcode: {:#x?}\n x: {}, z: {}, q: {}, y: {}, p: {}",
                opcode, x, z, q, y, p
            ),
        }
    }

    fn from_prefixed_byte(cpu: &Cpu) -> Self {
        let opcode = cpu.read_byte(cpu.register_pair(RegisterPair::PC) + 1);

        // https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        let x = (opcode >> 6) & 0b00000011;
        let y = (opcode >> 3) & 0b00000111;
        let z = opcode & 0b00000111;
        let p = y >> 1;
        let q = y & 0b00000001;

        match x {
            0 => Table::rot(y, z),
            1 => Self::BIT(y, Table::r(z)), // BIT y, r[z]
            2 => Self::RES(y, Table::r(z)), // RES y, r[z]
            3 => Self::SET(y, Table::r(z)), // SET y, r[z]
            _ => panic!(
                "Unknown Prefixed Opcode: 0xCB {:#x?}\n x: {}, z: {}, q: {}, y: {}, p: {}",
                opcode, x, z, q, y, p
            ),
        }
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
    ByteAtAddressWithOffset(u8),
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
    type Error = &'static str; // FIXME: Proper error type goes here.

    fn try_from(pair: InstrRegisterPair) -> Result<Self, Self::Error> {
        match pair {
            InstrRegisterPair::AF => Ok(Self::AF),
            InstrRegisterPair::BC => Ok(Self::BC),
            InstrRegisterPair::DE => Ok(Self::DE),
            InstrRegisterPair::HL => Ok(Self::HL),
            InstrRegisterPair::SP => Ok(Self::SP),
            InstrRegisterPair::PC => Ok(Self::PC),
            InstrRegisterPair::IncrementHL => {
                Err("Can not convert InstrRegisterPair::IncrementHL to RegisterPair")
            }
            InstrRegisterPair::DecrementHL => {
                Err("Can not convert InstrRegisterPair::DecrementHL to RegisterPair")
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
    type Error = &'static str; // FIXME: Proper error type goes here

    fn try_from(register: Register) -> Result<Self, Self::Error> {
        match register {
            Register::A => Ok(Self::A),
            Register::B => Ok(Self::B),
            Register::C => Ok(Self::C),
            Register::D => Ok(Self::D),
            Register::E => Ok(Self::E),
            Register::H => Ok(Self::H),
            Register::L => Ok(Self::L),
            Register::Flag => Err("Can not convert Register::Flag to InstrRegister"),
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

    pub fn x2_alu(index: u8, r_index: u8) -> Instruction {
        match index {
            0 => Instruction::ADD(
                // ADD A, r[z]
                MATHTarget::Register(InstrRegister::A),
                MATHTarget::Register(Self::r(r_index)),
            ),
            1 => Instruction::ADC(MATHTarget::Register(Self::r(r_index))), // ADC A, r[z]
            2 => Instruction::SUB(MATHTarget::Register(Self::r(r_index))), // SUB r[z]
            3 => Instruction::SBC(MATHTarget::Register(Self::r(r_index))), // SBC A, r[z]
            4 => Instruction::AND(MATHTarget::Register(Self::r(r_index))), // AND r[z]
            5 => Instruction::XOR(MATHTarget::Register(Self::r(r_index))), // XOR r[z]
            6 => Instruction::OR(MATHTarget::Register(Self::r(r_index))),  // OR r[z]
            7 => Instruction::CP(MATHTarget::Register(Self::r(r_index))),  // CP r[z]
            _ => unreachable!("Index {} is out of bounds in alu[]"),
        }
    }

    pub fn x3_alu(index: u8, n: u8) -> Instruction {
        match index {
            0 => Instruction::ADD(
                // ADD A, n
                MATHTarget::Register(InstrRegister::A),
                MATHTarget::ImmediateByte(n),
            ),
            1 => Instruction::ADC(MATHTarget::ImmediateByte(n)), // ADC A, n
            2 => Instruction::SUB(MATHTarget::ImmediateByte(n)), // SUB n
            3 => Instruction::SBC(MATHTarget::ImmediateByte(n)), // SBC A, n
            4 => Instruction::AND(MATHTarget::ImmediateByte(n)), // AND n
            5 => Instruction::XOR(MATHTarget::ImmediateByte(n)), // XOR n
            6 => Instruction::OR(MATHTarget::ImmediateByte(n)),  // OR n
            7 => Instruction::CP(MATHTarget::ImmediateByte(n)),  // CP n
            _ => unreachable!("Index {} is out of bounds in alu[]"),
        }
    }

    pub fn rot(index: u8, r_index: u8) -> Instruction {
        match index {
            0 => Instruction::RLC(Self::r(r_index)),  // RLC r[z]
            1 => Instruction::RRC(Self::r(r_index)),  // RRC r[z]
            2 => Instruction::RL(Self::r(r_index)),   // RL r[z]
            3 => Instruction::RR(Self::r(r_index)),   // RR r[z]
            4 => Instruction::SLA(Self::r(r_index)),  // SLA r[z]
            5 => Instruction::SRA(Self::r(r_index)),  // SRA r[z]
            6 => Instruction::SWAP(Self::r(r_index)), // SWAP r[z]
            7 => Instruction::SRL(Self::r(r_index)),  // SRL r[z]
            _ => unreachable!("Index {} is out of bounds in rot[]"),
        }
    }
}
