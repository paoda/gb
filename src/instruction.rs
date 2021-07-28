use crate::bus::BusIo;
use crate::cpu::{Cpu, Flags, HaltState, ImeState, Register, RegisterPair};
use std::{convert::TryFrom, fmt::Debug};

#[derive(Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
pub(crate) enum Instruction {
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

#[derive(Clone, Copy)]
pub(crate) enum JPTarget {
    RegisterPair(RegisterPair),
    ImmediateWord(u16),
}

#[derive(Clone, Copy)]
pub(crate) enum Registers {
    Byte(InstrRegister),
    Word(RegisterPair),
}

#[derive(Clone, Copy)]
pub(crate) enum MATHTarget {
    Register(InstrRegister),
    RegisterPair(RegisterPair),
    ImmediateByte(u8),
}

#[derive(Clone, Copy)]
pub(crate) enum LDTarget {
    IndirectC,
    Register(InstrRegister),
    IndirectRegister(InstrRegisterPair),
    ByteAtAddress(u16),
    ImmediateWord(u16),
    ImmediateByte(u8),
    RegisterPair(RegisterPair),
    ByteAtAddressWithOffset(u8),
}

#[derive(Clone, Copy)]
pub(crate) enum InstrRegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
    IncrementHL,
    DecrementHL,
}

#[derive(Clone, Copy)]
pub(crate) enum InstrRegister {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    IndirectHL, // (HL)
}

#[derive(Clone, Copy)]
pub(crate) enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

#[derive(Debug)]
struct Table;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
#[repr(transparent)]
pub struct Cycle(u32);

impl Instruction {
    pub(crate) fn execute(cpu: &mut Cpu, instruction: Self) -> Cycle {
        match instruction {
            Instruction::NOP => Cycle::new(4),
            Instruction::LD(lhs, rhs) => match (lhs, rhs) {
                (LDTarget::ByteAtAddress(nn), LDTarget::RegisterPair(RegisterPair::SP)) => {
                    // LD (nn), SP | Put Stack Pointer at address nn
                    cpu.write_word(nn, cpu.register_pair(RegisterPair::SP));
                    Cycle::new(20)
                }
                (LDTarget::RegisterPair(pair), LDTarget::ImmediateWord(nn)) => {
                    // LD rp[p], nn | Put value nn into register pair
                    use RegisterPair::*;

                    match pair {
                        BC | DE | HL | SP => cpu.set_register_pair(pair, nn),
                        _ => unreachable!("There is no \"LD {:?}, nn\" instruction", pair),
                    }
                    Cycle::new(12)
                }
                (LDTarget::IndirectRegister(pair), LDTarget::Register(InstrRegister::A)) => {
                    let a = cpu.register(Register::A);
                    match pair {
                        InstrRegisterPair::BC | InstrRegisterPair::DE => {
                            // LD (BC), A | Put A into memory address BC
                            // LD (DE), A | Put A into memory address DE
                            let addr = cpu.register_pair(pair.to_register_pair());
                            cpu.write_byte(addr, a);
                        }
                        InstrRegisterPair::IncrementHL => {
                            // LD (HL+), A | Put A into byte at address HL, then increment HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, a);

                            cpu.set_register_pair(RegisterPair::HL, addr + 1);
                        }
                        InstrRegisterPair::DecrementHL => {
                            // LD (HL-), A | Put A into byte at address HL, then decrement HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, a);

                            cpu.set_register_pair(RegisterPair::HL, addr - 1);
                        }
                        _ => unreachable!("There is no \"LD ({:?}), A\" instruction", pair),
                    }
                    Cycle::new(8)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::IndirectRegister(pair)) => {
                    match pair {
                        InstrRegisterPair::BC | InstrRegisterPair::DE => {
                            // LD A, (BC) | Put value at address BC into A
                            // LD A, (DE) | Put value at address DE into A
                            let addr = cpu.register_pair(pair.to_register_pair());
                            let byte = cpu.read_byte(addr);
                            cpu.set_register(Register::A, byte);
                        }
                        InstrRegisterPair::IncrementHL => {
                            // LD A, (HL+) | Put value at address HL into A, then increment HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let byte = cpu.read_byte(addr);

                            cpu.set_register(Register::A, byte);
                            cpu.set_register_pair(RegisterPair::HL, addr + 1);
                        }
                        InstrRegisterPair::DecrementHL => {
                            // LD A, (HL-) | Put value at address HL into A, then increment HL
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let byte = cpu.read_byte(addr);
                            cpu.set_register(Register::A, byte);

                            cpu.set_register_pair(RegisterPair::HL, addr - 1);
                        }
                        _ => unreachable!("There is no \"LD A, ({:?})\" instruction", pair),
                    }
                    Cycle::new(8)
                }
                (LDTarget::Register(reg), LDTarget::ImmediateByte(n)) => {
                    // LD r[y], n | Store n in Register
                    use InstrRegister::*;

                    match reg {
                        A | B | C | D | E | H | L => {
                            cpu.set_register(reg.to_register(), n);
                            Cycle::new(8)
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            cpu.write_byte(addr, n);
                            Cycle::new(12)
                        }
                    }
                }
                (LDTarget::IndirectC, LDTarget::Register(InstrRegister::A)) => {
                    // LD (0xFF00 + C), A | Store value of register A at address 0xFF00 + C
                    let addr = 0xFF00 + cpu.register(Register::C) as u16;
                    cpu.write_byte(addr, cpu.register(Register::A));
                    Cycle::new(8)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::IndirectC) => {
                    let addr = 0xFF00 + cpu.register(Register::C) as u16;
                    let byte = cpu.read_byte(addr);
                    cpu.set_register(Register::A, byte);
                    Cycle::new(8)
                }
                (LDTarget::Register(lhs), LDTarget::Register(rhs)) => {
                    // LD r[y], r[z] | Store value of RHS Register in LHS Register
                    use InstrRegister::*;

                    match rhs {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(rhs.to_register());

                            match lhs {
                                B | C | D | E | H | L | A => {
                                    cpu.set_register(lhs.to_register(), right);
                                    Cycle::new(4)
                                }
                                IndirectHL => {
                                    let addr = cpu.register_pair(RegisterPair::HL);
                                    cpu.write_byte(addr, right);
                                    Cycle::new(8)
                                }
                            }
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = cpu.read_byte(addr);

                            match lhs {
                                B | C | D | E | H | L | A => {
                                    cpu.set_register(lhs.to_register(), right);
                                    Cycle::new(8)
                                }
                                IndirectHL => {
                                    unreachable!(
                                        "There is no \"LD ({:?}), ({:?})\" instruction",
                                        lhs, rhs
                                    )
                                }
                            }
                        }
                    }
                }
                (LDTarget::ByteAtAddressWithOffset(n), LDTarget::Register(InstrRegister::A)) => {
                    // LD (0xFF00 + n), A | Store register A at address (0xFF00 + n)
                    cpu.write_byte(0xFF00 + (n as u16), cpu.register(Register::A));
                    Cycle::new(12)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::ByteAtAddressWithOffset(n)) => {
                    // LD A, (0xFF00 + n) | Store value at address (0xFF00 + n) in register A
                    let byte = cpu.read_byte(0xFF00 + (n as u16));
                    cpu.set_register(Register::A, byte);
                    Cycle::new(12)
                }
                (
                    LDTarget::RegisterPair(RegisterPair::SP),
                    LDTarget::RegisterPair(RegisterPair::HL),
                ) => {
                    // LD SP, HL | Load Register HL into Register SP
                    cpu.set_register_pair(RegisterPair::SP, cpu.register_pair(RegisterPair::HL));
                    Cycle::new(8)
                }
                (LDTarget::ByteAtAddress(nn), LDTarget::Register(InstrRegister::A)) => {
                    cpu.write_byte(nn, cpu.register(Register::A));
                    Cycle::new(16)
                }
                (LDTarget::Register(InstrRegister::A), LDTarget::ByteAtAddress(nn)) => {
                    let byte = cpu.read_byte(nn);
                    cpu.set_register(Register::A, byte);
                    Cycle::new(16)
                }
                _ => unreachable!("There is no \"LD {:?}, {:?}\" instruction", lhs, rhs),
            },
            Instruction::STOP => Cycle::new(4),
            Instruction::JR(cond, offset) => {
                // JR cc[y - 4], d | If condition is true, then add d to current address and jump
                // JR d | Add d to current address and jump
                let flags: &Flags = cpu.flags();

                let prev = cpu.register_pair(RegisterPair::PC);
                let addr = prev.wrapping_add(offset as u16);

                match cond {
                    JumpCondition::Always => {
                        cpu.set_register_pair(RegisterPair::PC, addr);
                        Cycle::new(12)
                    }
                    JumpCondition::NotZero => {
                        if !flags.z() {
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Zero => {
                        if flags.z() {
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c() {
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Carry => {
                        if flags.c() {
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                }
            }
            Instruction::ADD(lhs, rhs) => match (lhs, rhs) {
                (MATHTarget::RegisterPair(RegisterPair::HL), MATHTarget::RegisterPair(pair)) => {
                    // ADD HL, rp[p] | add register pair to HL.
                    use RegisterPair::*;

                    let mut flags: Flags = *cpu.flags();

                    match pair {
                        BC | DE | HL | SP => {
                            let hl_value = cpu.register_pair(RegisterPair::HL);
                            let value = cpu.register_pair(pair);
                            let sum = Self::add_u16(hl_value, value, &mut flags);

                            cpu.set_register_pair(RegisterPair::HL, sum);
                        }
                        _ => unreachable!("There is no \"ADD HL, {:?}\" instruction", pair),
                    }
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
                (MATHTarget::Register(InstrRegister::A), MATHTarget::Register(reg)) => {
                    // ADD A, r[z] | Add (A + r[z]) to register A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let a_value = cpu.register(Register::A);

                    let (cycles, sum) = match reg {
                        B | C | D | E | H | L | A => {
                            let value = cpu.register(reg.to_register());

                            let sum = Self::add(a_value, value, &mut flags);
                            (Cycle::new(4), sum)
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let value = cpu.read_byte(addr);

                            let sum = Self::add(a_value, value, &mut flags);
                            (Cycle::new(8), sum)
                        }
                    };

                    cpu.set_register(Register::A, sum);
                    cpu.set_flags(flags);
                    cycles
                }
                (MATHTarget::RegisterPair(RegisterPair::SP), MATHTarget::ImmediateByte(d)) => {
                    // ADD SP, d | Add d (is signed) to register pair SP.
                    let mut flags: Flags = *cpu.flags();
                    let d = d as i8;

                    let sum = Self::add_u16_i8(cpu.register_pair(RegisterPair::SP), d, &mut flags);

                    cpu.set_flags(flags);
                    cpu.set_register_pair(RegisterPair::SP, sum);
                    Cycle::new(16)
                }
                (MATHTarget::Register(InstrRegister::A), MATHTarget::ImmediateByte(n)) => {
                    // ADD A, n | Add n to register A
                    let mut flags: Flags = *cpu.flags();
                    let sum = Self::add(cpu.register(Register::A), n, &mut flags);

                    cpu.set_register(Register::A, sum);
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"ADD {:?}, {:?}\" instruction", lhs, rhs),
            },
            Instruction::INC(registers) => {
                match registers {
                    Registers::Byte(reg) => {
                        // INC r[y] | Increment Register
                        use InstrRegister::*;

                        let mut flags: Flags = *cpu.flags();

                        let cycles = match reg {
                            B | C | D | E | H | L | A => {
                                let reg = reg.to_register();

                                let value = cpu.register(reg);
                                cpu.set_register(reg, Self::inc_register(value, &mut flags));
                                Cycle::new(4)
                            }
                            IndirectHL => {
                                let addr = cpu.register_pair(RegisterPair::HL);
                                let byte = Self::inc_register(cpu.read_byte(addr), &mut flags);
                                cpu.write_byte(addr, byte);
                                Cycle::new(12)
                            }
                        };

                        cpu.set_flags(flags);
                        cycles
                    }
                    Registers::Word(pair) => {
                        // INC rp[p] | Increment Register Pair
                        // Note: According to RGBDS, no flags are set here.
                        use RegisterPair::*;

                        match pair {
                            BC | DE | HL | SP => {
                                let value = cpu.register_pair(pair);
                                cpu.set_register_pair(pair, value.wrapping_add(1));
                            }
                            _ => unreachable!("There is no \"INC {:?}\" instruction", pair),
                        }
                        Cycle::new(8)
                    }
                }
            }
            Instruction::DEC(registers) => {
                match registers {
                    Registers::Byte(reg) => {
                        // DEC r[y] | Decrement Register
                        use InstrRegister::*;

                        let mut flags: Flags = *cpu.flags();

                        let cycles = match reg {
                            B | C | D | E | H | L | A => {
                                let reg = reg.to_register();

                                let value = cpu.register(reg);
                                cpu.set_register(reg, Self::dec_register(value, &mut flags));
                                Cycle::new(4)
                            }
                            IndirectHL => {
                                let addr = cpu.register_pair(RegisterPair::HL);
                                let byte = cpu.read_byte(addr);
                                cpu.write_byte(addr, Self::dec_register(byte, &mut flags));
                                Cycle::new(12)
                            }
                        };

                        cpu.set_flags(flags);
                        cycles
                    }
                    Registers::Word(pair) => {
                        // DEC rp[p] | Decrement Register Pair
                        use RegisterPair::*;

                        match pair {
                            BC | DE | HL | SP => {
                                let value = cpu.register_pair(pair);
                                cpu.set_register_pair(pair, value.wrapping_sub(1));
                            }
                            _ => unreachable!("There is no \"DEC {:?}\" instruction", pair),
                        };
                        Cycle::new(8)
                    }
                }
            }
            Instruction::RLCA => {
                // Rotate Register A left
                let mut flags: Flags = *cpu.flags();

                let a = cpu.register(Register::A);
                let msb = a >> 7;
                let rot_a = a.rotate_left(1);

                flags.update(false, false, false, msb == 0x01);
                cpu.set_flags(flags);
                cpu.set_register(Register::A, rot_a);
                Cycle::new(4)
            }
            Instruction::RRCA => {
                // Rotate Register A right
                let mut flags: Flags = *cpu.flags();

                let a = cpu.register(Register::A);
                let lsb = a & 0x01;
                let rot_a = a.rotate_right(1);

                flags.update(false, false, false, lsb == 0x01);
                cpu.set_flags(flags);
                cpu.set_register(Register::A, rot_a);
                Cycle::new(4)
            }
            Instruction::RLA => {
                // Rotate register A left through carry
                let mut flags: Flags = *cpu.flags();

                let a = cpu.register(Register::A);
                let (rot_a, carry) = Self::rl_thru_carry(a, flags.c());

                flags.update(false, false, false, carry);
                cpu.set_flags(flags);
                cpu.set_register(Register::A, rot_a);
                Cycle::new(4)
            }
            Instruction::RRA => {
                // Rotate register A right through carry
                let mut flags: Flags = *cpu.flags();

                let a = cpu.register(Register::A);
                let (rot_a, carry) = Self::rr_thru_carry(a, flags.c());

                flags.update(false, false, false, carry);
                cpu.set_flags(flags);
                cpu.set_register(Register::A, rot_a);
                Cycle::new(4)
            }
            Instruction::DAA => {
                // source: https://ehaskins.com/2018-01-30%20Z80%20DAA/
                // TODO: Maybe i16 isn't the right choice here?

                let mut correction: i16 = 0;
                let mut value = cpu.register(Register::A) as i16;
                let mut flags = *cpu.flags();

                if flags.h() || (!flags.n() && (value & 0xF) > 9) {
                    correction |= 0x06;
                }

                if flags.c() || (!flags.n() && value > 0x99) {
                    correction |= 0x60;
                    flags.set_c(true);
                }

                value += if flags.n() { -correction } else { correction };
                let result = value as u8;

                flags.set_z(result == 0);
                flags.set_h(false);

                cpu.set_flags(flags);
                cpu.set_register(Register::A, value as u8);

                Cycle::new(4)
            }
            Instruction::CPL => {
                // Compliment A register (inverse)
                let mut flags: Flags = *cpu.flags();
                let a = cpu.register(Register::A);

                flags.set_n(true);
                flags.set_h(true);

                cpu.set_flags(flags);
                cpu.set_register(Register::A, !a); // Bitwise not is ! instead of ~
                Cycle::new(4)
            }
            Instruction::SCF => {
                // Set Carry Flag
                let mut flags: Flags = *cpu.flags();

                flags.set_n(false);
                flags.set_h(false);
                flags.set_c(true);

                cpu.set_flags(flags);
                Cycle::new(4)
            }
            Instruction::CCF => {
                // Compliment Carry Flag (inverse)
                let mut flags: Flags = *cpu.flags();

                flags.set_n(false);
                flags.set_h(false);
                flags.set_c(!flags.c());

                cpu.set_flags(flags);
                Cycle::new(4)
            }
            Instruction::HALT => {
                // Enter CPU low power consumption mode until interrupt occurs
                use HaltState::*;

                let req = cpu.read_byte(0xFF0F);
                let enabled = cpu.read_byte(0xFFFF);

                let halt_state = if let ImeState::Enabled = cpu.ime() {
                    ImeEnabled
                } else if req & enabled != 0 {
                    SomePending
                } else {
                    NonePending
                };

                cpu.halt(halt_state);

                // Though this can actually last forever
                Cycle::new(4)
            }
            Instruction::ADC(target) => match target {
                MATHTarget::Register(reg) => {
                    // ADC A, r[z] | Add register r[z] plus the Carry flag to A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let left = cpu.register(Register::A);

                    let (cycles, sum) = match reg {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(reg.to_register());
                            let sum = Self::add_with_carry_bit(left, right, flags.c(), &mut flags);
                            (Cycle::new(4), sum)
                        }
                        IndirectHL => {
                            let right = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            let sum = Self::add_with_carry_bit(left, right, flags.c(), &mut flags);
                            (Cycle::new(8), sum)
                        }
                    };

                    cpu.set_register(Register::A, sum);
                    cpu.set_flags(flags);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // ADC A, n | Add immediate byte plus the carry flag to A
                    let mut flags: Flags = *cpu.flags();
                    let value = cpu.register(Register::A);

                    let sum = Self::add_with_carry_bit(value, n, flags.c(), &mut flags);

                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, sum);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"ADC {:?}\" instruction", target),
            },
            Instruction::SUB(target) => match target {
                MATHTarget::Register(reg) => {
                    // SUB r[z] | Subtract the value in register r[z] from register A, then store in A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let a_value = cpu.register(Register::A);

                    let (cycles, diff) = match reg {
                        B | C | D | E | H | L | A => {
                            let value = cpu.register(reg.to_register());
                            let diff = Self::sub(a_value, value, &mut flags);
                            (Cycle::new(4), diff)
                        }
                        IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            let diff = Self::sub(a_value, value, &mut flags);
                            (Cycle::new(8), diff)
                        }
                    };

                    cpu.set_register(Register::A, diff);
                    cpu.set_flags(flags);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // SUB n | Subtract the immediate byte from register A, then store in A
                    let mut flags: Flags = *cpu.flags();
                    let diff = Self::sub(cpu.register(Register::A), n, &mut flags);

                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, diff);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"SUB {:?}\" instruction", target),
            },
            Instruction::SBC(target) => match target {
                MATHTarget::Register(reg) => {
                    // SBC A, r[z] | Subtract the value from register r[z] from A, add the Carry flag and then store in A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let left = cpu.register(Register::A);

                    let (cycles, diff) = match reg {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(reg.to_register());
                            let diff = Self::sub_with_carry(left, right, flags.c(), &mut flags);
                            (Cycle::new(4), diff)
                        }
                        IndirectHL => {
                            let right = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            let diff = Self::sub_with_carry(left, right, flags.c(), &mut flags);

                            (Cycle::new(8), diff)
                        }
                    };

                    cpu.set_register(Register::A, diff);
                    cpu.set_flags(flags);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // SBC A, n | Subtract the value from immediate byte from A, add the carry flag and then store in A
                    let mut flags: Flags = *cpu.flags();
                    let value = cpu.register(Register::A);

                    let diff = Self::sub_with_carry(value, n, flags.c(), &mut flags);

                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, diff);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"SBC {:?}\" instruction", target),
            },
            Instruction::AND(target) => match target {
                MATHTarget::Register(reg) => {
                    // AND r[z] | Bitwise AND register r[z] and register A, store in register A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let a_value = cpu.register(Register::A);

                    let (cycles, result) = match reg {
                        B | C | D | E | H | L | A => {
                            let value = cpu.register(reg.to_register());
                            (Cycle::new(4), a_value & value)
                        }
                        IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            (Cycle::new(8), a_value & value)
                        }
                    };

                    flags.update(result == 0, false, true, false);
                    cpu.set_register(Register::A, result);
                    cpu.set_flags(flags);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // AND n | Bitwise AND immediate byte and register A, store in register A
                    let mut flags: Flags = *cpu.flags();
                    let result = cpu.register(Register::A) & n;

                    flags.update(result == 0, false, true, false);
                    cpu.set_register(Register::A, result);
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"AND {:?}\" instruction", target),
            },
            Instruction::XOR(target) => match target {
                MATHTarget::Register(reg) => {
                    // XOR r[z] | Bitwise XOR register r[z] and register A, store in register A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let a_value = cpu.register(Register::A);

                    let (cycles, result) = match reg {
                        B | C | D | E | H | L | A => {
                            let value = cpu.register(reg.to_register());
                            (Cycle::new(4), a_value ^ value)
                        }
                        IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            (Cycle::new(8), a_value ^ value)
                        }
                    };

                    flags.update(result == 0, false, false, false);
                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, result);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // XOR n | Bitwise XOR immediate byte and register A, store in register A
                    let mut flags: Flags = *cpu.flags();
                    let result = cpu.register(Register::A) ^ n;

                    flags.update(result == 0, false, false, false);
                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, result);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"XOR {:?}\" instruction", target),
            },
            Instruction::OR(target) => match target {
                MATHTarget::Register(reg) => {
                    // OR r[z] | Bitwise OR register r[z] and register A, store in register A
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let a_value = cpu.register(Register::A);

                    let (cycles, result) = match reg {
                        B | C | D | E | H | L | A => {
                            let value = cpu.register(reg.to_register());
                            (Cycle::new(4), a_value | value)
                        }
                        IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            (Cycle::new(8), a_value | value)
                        }
                    };

                    flags.update(result == 0, false, false, false);
                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, result);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // OR n | Bitwise OR on immediate byte n and register A, store in register A
                    let mut flags: Flags = *cpu.flags();
                    let result = cpu.register(Register::A) | n;

                    flags.update(result == 0, false, false, false);
                    cpu.set_flags(flags);
                    cpu.set_register(Register::A, result);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"OR {:?}\" instruction", target),
            },
            Instruction::CP(target) => match target {
                MATHTarget::Register(reg) => {
                    // CP r[z] | Same behaviour as SUB, except the result is not stored.
                    use InstrRegister::*;

                    let mut flags: Flags = *cpu.flags();
                    let a_value = cpu.register(Register::A);

                    let cycles = match reg {
                        B | C | D | E | H | L | A => {
                            let value = cpu.register(reg.to_register());
                            let _ = Self::sub(a_value, value, &mut flags);
                            Cycle::new(4)
                        }
                        IndirectHL => {
                            let value = cpu.read_byte(cpu.register_pair(RegisterPair::HL));
                            let _ = Self::sub(a_value, value, &mut flags);
                            Cycle::new(8)
                        }
                    };

                    cpu.set_flags(flags);
                    cycles
                }
                MATHTarget::ImmediateByte(n) => {
                    // CP n | Same behaviour as SUB, except the result is not stored,
                    let mut flags: Flags = *cpu.flags();
                    let _ = Self::sub(cpu.register(Register::A), n, &mut flags);

                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
                _ => unreachable!("There is no \"CP {:?}\" instruction", target),
            },
            Instruction::RET(cond) => {
                // RET cc[y] | Essentially a POP PC, Return from Subroutine
                // RET       | Essentially a POP PC, Return from Subroutine
                let flags: &Flags = cpu.flags();

                match cond {
                    JumpCondition::NotZero => {
                        if !flags.z() {
                            let addr = Self::pop(cpu);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Zero => {
                        if flags.z() {
                            let addr = Self::pop(cpu);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c() {
                            let addr = Self::pop(cpu);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Carry => {
                        if flags.c() {
                            let addr = Self::pop(cpu);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Always => {
                        let addr = Self::pop(cpu);
                        cpu.set_register_pair(RegisterPair::PC, addr);
                        Cycle::new(16)
                    }
                }
            }
            Instruction::LDHL(d) => {
                // LDHL SP + d   | Add SP + d to register HL
                // LD HL, SP + d | Add SP + d to register HL
                let mut flags: Flags = *cpu.flags();
                let sum = Self::add_u16_i8(cpu.register_pair(RegisterPair::SP), d, &mut flags);

                cpu.set_register_pair(RegisterPair::HL, sum);
                cpu.set_flags(flags);
                Cycle::new(12)
            }
            Instruction::POP(pair) => {
                // POP rp2[p] | Pop from stack into register pair rp2[]
                // Flags are set when we call cpu.set_register_pair(RegisterPair::AF, value);
                use RegisterPair::*;

                match pair {
                    BC | DE | HL | AF => {
                        let value = Self::pop(cpu);
                        cpu.set_register_pair(pair, value);
                    }
                    _ => unreachable!("There is no \"POP {:?}\" instruction", pair),
                }
                Cycle::new(12)
            }
            Instruction::RETI => {
                // Same as RET, after which interrupts are enabled.
                let addr = Self::pop(cpu);
                cpu.set_register_pair(RegisterPair::PC, addr);
                cpu.set_ime(ImeState::Enabled);
                Cycle::new(16)
            }
            Instruction::JP(cond, target) => match target {
                JPTarget::RegisterPair(RegisterPair::HL) => {
                    // JP HL | Load register pair HL into program counter
                    let addr = cpu.register_pair(RegisterPair::HL);

                    cpu.set_register_pair(RegisterPair::PC, addr);
                    Cycle::new(4)
                }
                JPTarget::ImmediateWord(nn) => {
                    // JP cc[y], nn | Store Immediate Word in the Program Counter if cond is met
                    // JP nn        | Store Immediate Word in the Program Counter
                    let flags: &Flags = cpu.flags();

                    match cond {
                        JumpCondition::NotZero => {
                            if !flags.z() {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::Zero => {
                            if flags.z() {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::NotCarry => {
                            if !flags.c() {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::Carry => {
                            if flags.c() {
                                cpu.set_register_pair(RegisterPair::PC, nn);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::Always => {
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            Cycle::new(16)
                        }
                    }
                }
                _ => unreachable!("There is no \"JP {:?}\" instruction", target),
            },
            Instruction::DI => {
                // Disable IME
                cpu.set_ime(ImeState::Disabled);
                Cycle::new(4)
            }
            Instruction::EI => {
                // Enable IME (After the next instruction)
                // FIXME: IME is set after the next instruction, this currently is not represented in this emulator.
                cpu.set_ime(ImeState::Pending);
                Cycle::new(4)
            }
            Instruction::CALL(cond, nn) => {
                // CALL cc[y], nn | Store pc on the stack, then store nn in the program counter if cond is met
                // CALL nn        | Store nn on the stack, then store nn in the program counter
                let flags: &Flags = cpu.flags();
                let pc = cpu.register_pair(RegisterPair::PC);

                match cond {
                    JumpCondition::NotZero => {
                        if !flags.z() {
                            Self::push(cpu, pc);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::Zero => {
                        if flags.z() {
                            Self::push(cpu, pc);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c() {
                            Self::push(cpu, pc);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::Carry => {
                        if flags.c() {
                            Self::push(cpu, pc);
                            cpu.set_register_pair(RegisterPair::PC, nn);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::Always => {
                        Self::push(cpu, pc);
                        cpu.set_register_pair(RegisterPair::PC, nn);
                        Cycle::new(24)
                    }
                }
            }
            Instruction::PUSH(pair) => {
                // PUSH rp2[p] | Push register pair onto the stack
                use RegisterPair::*;

                match pair {
                    BC | DE | HL | AF => {
                        let value = cpu.register_pair(pair);
                        Self::push(cpu, value);
                    }
                    _ => unreachable!("There is no \"PUSH {:?}\" instruction", pair),
                }
                Cycle::new(16)
            }
            Instruction::RST(n) => {
                // RST n | Push current address onto the stack, jump to 0x0000 + n

                // The same behaviour will occur when handling an interrupt so this code
                // is relegated to a method
                Self::reset(cpu, n)
            }
            Instruction::RLC(reg) => {
                // RLC r[z] | Rotate register r[z] left
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, msb, rotated) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();

                        let value = cpu.register(register);
                        let rotated = value.rotate_left(1);

                        cpu.set_register(register, rotated);
                        (Cycle::new(8), value >> 7, rotated)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);

                        let value = cpu.read_byte(addr);
                        let rotated = value.rotate_left(1);

                        cpu.write_byte(addr, rotated);
                        (Cycle::new(16), value >> 7, rotated)
                    }
                };

                flags.update(rotated == 0, false, false, msb == 0x01);
                cpu.set_flags(flags);
                cycles
            }
            Instruction::RRC(reg) => {
                // RRC r[z] | Rotate Register r[z] right
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, lsb, rotated) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();

                        let value = cpu.register(register);
                        let rotated = value.rotate_right(1);

                        cpu.set_register(register, rotated);
                        (Cycle::new(8), value & 0x01, rotated)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);

                        let value = cpu.read_byte(addr);
                        let rotated = value.rotate_right(1);

                        cpu.write_byte(addr, rotated);
                        (Cycle::new(16), value & 0x01, rotated)
                    }
                };

                flags.update(rotated == 0, false, false, lsb == 0x01);
                cpu.set_flags(flags);
                cycles
            }
            Instruction::RL(reg) => {
                // RL r[z] | Rotate register r[z] left through carry
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, rotated, carry) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let (rotated, carry) = Self::rl_thru_carry(value, flags.c());

                        cpu.set_register(register, rotated);
                        (Cycle::new(8), rotated, carry)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let (rotated, carry) = Self::rl_thru_carry(value, flags.c());

                        cpu.write_byte(addr, rotated);
                        (Cycle::new(16), rotated, carry)
                    }
                };

                flags.update(rotated == 0, false, false, carry);
                cpu.set_flags(flags);

                cycles
            }
            Instruction::RR(reg) => {
                // RR r[z] | Rotate register r[z] right through carry
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, rotated, carry) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let (rotated, carry) = Self::rr_thru_carry(value, flags.c());

                        cpu.set_register(register, rotated);
                        (Cycle::new(8), rotated, carry)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let (rotated, carry) = Self::rr_thru_carry(value, flags.c());

                        cpu.write_byte(addr, rotated);
                        (Cycle::new(16), rotated, carry)
                    }
                };

                flags.update(rotated == 0, false, false, carry);
                cpu.set_flags(flags);

                cycles
            }
            Instruction::SLA(reg) => {
                // SLA r[z] | Shift left arithmetic register r[z]
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, msb, shifted) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let shifted = value << 1;

                        cpu.set_register(register, shifted);
                        (Cycle::new(8), (value >> 7) & 0x01, shifted)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let shifted = value << 1;

                        cpu.write_byte(addr, shifted);
                        (Cycle::new(16), (value >> 7) & 0x01, shifted)
                    }
                };

                flags.update(shifted == 0, false, false, msb == 0x01);
                cpu.set_flags(flags);

                cycles
            }
            Instruction::SRA(reg) => {
                // SRA r[z] | Shift right arithmetic register r[z]
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, lsb, shifted) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let msb = (value >> 7) & 0x01;
                        let shifted = msb << 7 | value >> 1;

                        cpu.set_register(register, shifted);
                        (Cycle::new(8), value & 0x01, shifted)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let msb = (value >> 7) & 0x01;
                        let shifted = msb << 7 | value >> 1;

                        cpu.write_byte(addr, shifted);
                        (Cycle::new(16), value & 0x01, shifted)
                    }
                };

                flags.update(shifted == 0, false, false, lsb == 0x01);
                cpu.set_flags(flags);

                cycles
            }
            Instruction::SWAP(reg) => {
                // SWAP r[z] | Swap the 4 highest and lowest bits in a byte
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, swapped) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let swapped = Self::swap_bits(value);

                        cpu.set_register(register, swapped);
                        (Cycle::new(8), swapped)
                    }

                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let swapped = Self::swap_bits(value);

                        cpu.write_byte(addr, swapped);
                        (Cycle::new(16), swapped)
                    }
                };

                flags.update(swapped == 0, false, false, false);
                cpu.set_flags(flags);

                cycles
            }
            Instruction::SRL(reg) => {
                // SRL r[z] | Shift right logic register r[z]
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, lsb, shift_reg) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let shifted = value >> 1;

                        cpu.set_register(register, shifted);
                        (Cycle::new(8), value & 0x01, shifted)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let shifted = value >> 1;

                        cpu.write_byte(addr, shifted);
                        (Cycle::new(16), value & 0x01, shifted)
                    }
                };

                flags.update(shift_reg == 0, false, false, lsb == 0x01);
                cpu.set_flags(flags);

                cycles
            }
            Instruction::BIT(y, reg) => {
                // BIT y, r[z] | Test y is in register r[z]
                use InstrRegister::*;

                let mut flags: Flags = *cpu.flags();

                let (cycles, is_bit_set) = match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        let is_bit_set = ((value >> y) & 0x01) == 0x01;
                        (Cycle::new(8), is_bit_set)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        let is_bit_set = ((value >> y) & 0x01) == 0x01;
                        (Cycle::new(12), is_bit_set)
                    }
                };

                flags.update(!is_bit_set, false, true, flags.c());
                cpu.set_flags(flags);

                cycles
            }
            Instruction::RES(y, reg) => {
                // RES y, r[z] | Reset bit y to zero
                //
                // 00000001 << 3 = 00001000
                // ~00001000 = 11110111
                // value & 11110111 means that only a specific bit will be reset
                use InstrRegister::*;

                match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        cpu.set_register(register, value & !(1u8 << y));
                        Cycle::new(8)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        cpu.write_byte(addr, value & !(1u8 << y));
                        Cycle::new(16)
                    }
                }
            }
            Instruction::SET(y, reg) => {
                // BIT y, r[z] | Set bit y to one
                //
                // 00000001 << 3 = 00001000
                // value | 00001000 means that only a specific bit will be set
                use InstrRegister::*;

                match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.to_register();
                        let value = cpu.register(register);

                        cpu.set_register(register, value | (1u8 << y));
                        Cycle::new(8)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let value = cpu.read_byte(addr);

                        cpu.write_byte(addr, value | (1u8 << y));
                        Cycle::new(16)
                    }
                }
            }
        }
    }

    /// PUSHes a u16 onto the stack
    ///
    /// Mutates the stack pointer and the stack
    fn push(cpu: &mut Cpu, value: u16) {
        let mut sp = cpu.register_pair(RegisterPair::SP);

        sp -= 1;
        cpu.write_byte(sp, (value >> 8) as u8);
        sp -= 1;
        cpu.write_byte(sp, value as u8);

        cpu.set_register_pair(RegisterPair::SP, sp);
    }

    /// POPs a u16 from the stack
    ///
    /// Mutates the stack pointer and returns the u16 which was popped from the stack
    fn pop(cpu: &mut Cpu) -> u16 {
        let mut sp = cpu.register_pair(RegisterPair::SP);

        let low = cpu.read_byte(sp);
        sp += 1;
        let high = cpu.read_byte(sp);
        sp += 1;

        cpu.set_register_pair(RegisterPair::SP, sp);
        (high as u16) << 8 | low as u16
    }

    fn dec_register(byte: u8, flags: &mut Flags) -> u8 {
        Self::sub_no_carry(byte, 1, flags)
    }

    fn inc_register(byte: u8, flags: &mut Flags) -> u8 {
        Self::add_no_carry(byte, 1, flags)
    }

    fn sub_no_carry(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let diff = left.wrapping_sub(right);

        flags.set_z(diff == 0);
        flags.set_n(true);
        flags.set_h(Self::bit_4_borrow(left, right));

        diff
    }

    fn sub(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let (diff, did_overflow) = left.overflowing_sub(right);

        flags.update(
            diff == 0,
            true,
            Self::bit_4_borrow(left, right),
            did_overflow,
        );
        diff
    }

    fn sub_with_carry(left: u8, right: u8, carry: bool, flags: &mut Flags) -> u8 {
        let carry = carry as u8;

        let (diff, did_overflow) = {
            let (tmp_diff, did) = left.overflowing_sub(right);
            let (diff, overflow) = tmp_diff.overflowing_sub(carry);

            (diff, did || overflow)
        };

        flags.update(
            diff == 0,
            true,
            (left & 0x0F).wrapping_sub(right & 0x0F).wrapping_sub(carry) > 0x0F,
            did_overflow,
        );

        diff
    }

    fn add_u16_i8(left: u16, right: i8, flags: &mut Flags) -> u16 {
        let (_, did_overflow) = (left as u8).overflowing_add(right as u8);
        let sum = left.wrapping_add(right as u16);

        let half_carry = Self::bit_3_overflow(left as u8, right as u8);
        flags.update(false, false, half_carry, did_overflow);
        sum
    }

    fn add_no_carry(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let sum = left.wrapping_add(right);

        flags.set_z(sum == 0);
        flags.set_n(false);
        flags.set_h(Self::bit_3_overflow(left, right));
        sum
    }

    fn add(left: u8, right: u8, flags: &mut Flags) -> u8 {
        let (sum, did_overflow) = left.overflowing_add(right);

        flags.update(
            sum == 0,
            false,
            Self::bit_3_overflow(left, right),
            did_overflow,
        );

        sum
    }

    fn add_with_carry_bit(left: u8, right: u8, carry: bool, flags: &mut Flags) -> u8 {
        let carry = carry as u8;

        let (sum, did_overflow) = {
            let (tmp_sum, did) = left.overflowing_add(right);
            let (sum, overflow) = tmp_sum.overflowing_add(carry);

            (sum, did || overflow)
        };

        flags.update(
            sum == 0,
            false,
            (((left & 0x0F) + (right & 0x0F) + carry) & 0x10) == 0x10,
            did_overflow,
        );

        sum
    }

    fn add_u16(left: u16, right: u16, flags: &mut Flags) -> u16 {
        let (sum, did_overflow) = left.overflowing_add(right);

        flags.set_n(false);
        flags.set_h(Self::bit_11_overflow(left, right));
        flags.set_c(did_overflow);

        sum
    }

    fn bit_11_overflow(left: u16, right: u16) -> bool {
        (((left & 0x0FFF) + (right & 0x0FFF)) & 0x1000) == 0x1000
    }

    fn bit_3_overflow(left: u8, right: u8) -> bool {
        (((left & 0xF) + (right & 0xF)) & 0x10) == 0x10
    }

    fn bit_4_borrow(left: u8, right: u8) -> bool {
        (left & 0x0F) < (right & 0x0F)
    }

    fn rl_thru_carry(byte: u8, carry: bool) -> (u8, bool) {
        let carry_flag = (byte >> 7) & 0x01; // get the MSB of the u8 (which will rotate into the carry bit)
        let new_byte = (byte << 1) | carry as u8; // shift the bit left, and then OR the carry bit in.

        (new_byte, carry_flag == 0x01)
    }

    fn rr_thru_carry(byte: u8, carry: bool) -> (u8, bool) {
        let carry_flag = byte & 0x01; // get the LSB of the u8 (which will rotate into the carry bit)
        let new_byte = ((carry as u8) << 7) | (byte >> 1); // shift the bit right, and then OR the carry bit in.

        (new_byte, carry_flag == 0x01)
    }

    fn swap_bits(byte: u8) -> u8 {
        let upper = byte >> 4;
        let lower = byte & 0x0F;

        (lower << 4) | upper
    }

    pub(crate) fn reset(cpu: &mut Cpu, vector: u8) -> Cycle {
        let addr = cpu.register_pair(RegisterPair::PC);
        Self::push(cpu, addr);
        cpu.set_register_pair(RegisterPair::PC, vector as u16);
        Cycle::new(16)
    }
}

impl Instruction {
    pub(crate) fn from_byte(cpu: &mut Cpu, byte: u8) -> Self {
        if byte == 0xCB {
            Self::from_prefixed_byte(cpu)
        } else {
            Self::from_unprefixed_byte(cpu, byte)
        }
    }

    fn from_unprefixed_byte(cpu: &mut Cpu, opcode: u8) -> Self {
        // https://gb-archive.github.io/salvage/decoding_gbz80_opcodes/Decoding%20Gamboy%20Z80%20Opcodes.html
        #![allow(clippy::many_single_char_names)]

        let x = (opcode >> 6) & 0x03;
        let y = (opcode >> 3) & 0x07;
        let z = opcode & 0x07;
        let p = y >> 1;
        let q = y & 0x01;

        let pc = cpu.register_pair(RegisterPair::PC);

        match (x, z, q, y, p) {
            (0, 0, _, 0, _) => Self::NOP, // NOP
            (0, 0, _, 1, _) => Self::LD(
                // LD (nn), SP
                LDTarget::ByteAtAddress(cpu.read_imm_word(pc)),
                LDTarget::RegisterPair(RegisterPair::SP),
            ),
            (0, 0, _, 2, _) => Self::STOP, // STOP
            (0, 0, _, 3, _) => Self::JR(JumpCondition::Always, cpu.read_imm_byte(pc) as i8), // JR d
            (0, 0, _, 4..=7, _) => Self::JR(Table::cc(y - 4), cpu.read_imm_byte(pc) as i8), // JR cc[y - 4], d
            (0, 1, 0, _, _) => Self::LD(
                // LD rp[p], nn
                LDTarget::RegisterPair(Table::rp(p)),
                LDTarget::ImmediateWord(cpu.read_imm_word(pc)),
            ),
            (0, 1, 1, _, _) => Self::ADD(
                // ADD HL, rp[p]
                MATHTarget::RegisterPair(RegisterPair::HL),
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
                LDTarget::ImmediateByte(cpu.read_imm_byte(pc)),
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
                LDTarget::ByteAtAddressWithOffset(cpu.read_imm_byte(pc)),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 0, _, 5, _) => Self::ADD(
                // ADD SP, d
                MATHTarget::RegisterPair(RegisterPair::SP),
                MATHTarget::ImmediateByte(cpu.read_imm_byte(pc)),
            ),
            (3, 0, _, 6, _) => Self::LD(
                // LD A, (0xFF00 + n)
                LDTarget::Register(InstrRegister::A),
                LDTarget::ByteAtAddressWithOffset(cpu.read_imm_byte(pc)),
            ),
            (3, 0, _, 7, _) => Self::LDHL(cpu.read_imm_byte(pc) as i8), // LD HL, SP + d
            (3, 1, 0, _, _) => Self::POP(Table::rp2(p)),                // POP rp2[p]
            (3, 1, 1, _, 0) => Self::RET(JumpCondition::Always),        // RET
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
                JPTarget::ImmediateWord(cpu.read_imm_word(pc)),
            ),
            (3, 2, _, 4, _) => Self::LD(
                // LD (0xFF00 + C) ,A
                LDTarget::IndirectC,
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 2, _, 5, _) => Self::LD(
                // LD (nn), A
                LDTarget::ByteAtAddress(cpu.read_imm_word(pc)),
                LDTarget::Register(InstrRegister::A),
            ),
            (3, 2, _, 6, _) => Self::LD(
                // LD A, (0xFF00 + C)
                LDTarget::Register(InstrRegister::A),
                LDTarget::IndirectC,
            ),
            (3, 2, _, 7, _) => Self::LD(
                // LD A, (nn)
                LDTarget::Register(InstrRegister::A),
                LDTarget::ByteAtAddress(cpu.read_imm_word(pc)),
            ),
            (3, 3, _, 0, _) => Self::JP(
                // JP nn
                JumpCondition::Always,
                JPTarget::ImmediateWord(cpu.read_imm_word(pc)),
            ),
            (3, 3, _, 1, _) => unreachable!("0xCB is handled by Instruction::from_prefixed_byte"),
            // (3, 3, _, 2, _) => unreachable!("\"removed\" in documentation"),
            // (3, 3, _, 3, _) => unreachable!("\"removed\" in documentation"),
            // (3, 3, _, 4, _) => unreachable!("\"removed\" in documentation"),
            // (3, 3, _, 5, _) => unreachable!("\"removed\" in documentation"),
            (3, 3, _, 6, _) => Self::DI,
            (3, 3, _, 7, _) => Self::EI,
            (3, 4, _, 0..=3, _) => Self::CALL(Table::cc(y), cpu.read_imm_word(pc)), // CALL cc[y], nn
            // (3, 4, _, 4..=7, _) => unreachable!("\"removed\" in documentation"),
            (3, 5, 0, _, _) => Self::PUSH(Table::rp2(p)), // PUSH rp2[p]
            (3, 5, 1, _, 0) => Self::CALL(JumpCondition::Always, cpu.read_imm_word(pc)), // CALL nn
            // (3, 5, 1, _, 1..=3) => unreachable!("\"removed\" in documentation"),
            (3, 6, _, _, _) => Table::x3_alu(y, cpu.read_imm_byte(pc)),
            (3, 7, _, _, _) => Self::RST(y * 8), // RST n
            _ => unreachable!(
                "Unknown Opcode: {:#04X}\n x: {}, z: {}, q: {}, y: {}, p: {}",
                opcode, x, z, q, y, p
            ),
        }
    }

    fn from_prefixed_byte(cpu: &mut Cpu) -> Self {
        #![allow(clippy::many_single_char_names)]

        let pc = cpu.register_pair(RegisterPair::PC);
        let opcode = cpu.read_imm_byte(pc); // FIXME: Should the PC be incremented here?

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
            _ => unreachable!(
                "Unknown Prefixed Opcode: 0xCB {:#04X}\n x: {}, z: {}, q: {}, y: {}, p: {}",
                opcode, x, z, q, y, p
            ),
        }
    }
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
        }
    }
}

impl Table {
    fn r(index: u8) -> InstrRegister {
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

    fn rp2(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::AF,
            _ => unreachable!("Index {} out of bounds in rp2[]", index),
        }
    }

    fn rp(index: u8) -> RegisterPair {
        match index {
            0 => RegisterPair::BC,
            1 => RegisterPair::DE,
            2 => RegisterPair::HL,
            3 => RegisterPair::SP,
            _ => unreachable!("Index {} out of bounds in rp[]", index),
        }
    }

    fn cc(index: u8) -> JumpCondition {
        match index {
            0 => JumpCondition::NotZero,
            1 => JumpCondition::Zero,
            2 => JumpCondition::NotCarry,
            3 => JumpCondition::Carry,
            _ => unreachable!("Index {} out of bounds in cc[]", index),
        }
    }

    fn x2_alu(index: u8, r_index: u8) -> Instruction {
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

    fn x3_alu(index: u8, n: u8) -> Instruction {
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

    fn rot(index: u8, r_index: u8) -> Instruction {
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

impl std::fmt::Debug for JPTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            JPTarget::RegisterPair(pair) => write!(f, "{:?}", pair),
            JPTarget::ImmediateWord(word) => write!(f, "{:#06X}", word),
        }
    }
}

impl std::fmt::Debug for LDTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            LDTarget::IndirectC => f.write_str("(0xFF00 + C)"),
            LDTarget::Register(reg) => write!(f, "{:?}", reg),
            LDTarget::IndirectRegister(pair) => write!(f, "({:?})", pair),
            LDTarget::ByteAtAddress(addr) => write!(f, "({:#06X})", addr),
            LDTarget::ImmediateWord(word) => write!(f, "{:#06X}", word),
            LDTarget::ImmediateByte(byte) => write!(f, "{:#04X}", byte),
            LDTarget::RegisterPair(pair) => write!(f, "{:?}", pair),
            LDTarget::ByteAtAddressWithOffset(byte) => {
                write!(f, "(0xFF00 + {:#04X})", byte)
            }
        }
    }
}

impl std::fmt::Debug for MATHTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            MATHTarget::Register(reg) => write!(f, "{:?}", reg),
            MATHTarget::RegisterPair(pair) => write!(f, "{:?}", pair),
            MATHTarget::ImmediateByte(byte) => write!(f, "{:#04X}", byte),
        }
    }
}

impl std::fmt::Debug for InstrRegisterPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            InstrRegisterPair::AF => f.write_str("AF"),
            InstrRegisterPair::BC => f.write_str("BC"),
            InstrRegisterPair::DE => f.write_str("DE"),
            InstrRegisterPair::HL => f.write_str("HL"),
            InstrRegisterPair::SP => f.write_str("SP"),
            InstrRegisterPair::PC => f.write_str("PC"),
            InstrRegisterPair::IncrementHL => f.write_str("HL+"),
            InstrRegisterPair::DecrementHL => f.write_str("HL-"),
        }
    }
}

impl std::fmt::Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Registers::Byte(reg) => write!(f, "{:?}", reg),
            Registers::Word(pair) => write!(f, "{:?}", pair),
        }
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;

        match *self {
            NOP => f.write_str("NOP"),
            LD(left, right) => write!(f, "LD {:?}, {:?}", left, right),
            STOP => f.write_str("STOP"),
            JR(cond, dist) => write!(f, "JR {:?}, {:?}", cond, dist),
            ADD(left, right) => write!(f, "ADD {:?}, {:?}", left, right),
            INC(register) => write!(f, "INC {:?}", register),
            DEC(register) => write!(f, "DEC {:?}", register),
            RLCA => f.write_str("RLCA"),
            RRCA => f.write_str("RRCA"),
            RLA => f.write_str("RLA"),
            RRA => f.write_str("RRA"),
            DAA => f.write_str("DAA"),
            CPL => f.write_str("CPL"),
            SCF => f.write_str("SCF"),
            CCF => f.write_str("CCF"),
            HALT => f.write_str("HALT"),
            ADC(target) => write!(f, "ADC {:?}", target),
            SUB(target) => write!(f, "SUB {:?}", target),
            SBC(target) => write!(f, "SBC {:?}", target),
            AND(target) => write!(f, "AND {:?}", target),
            XOR(target) => write!(f, "XOR {:?}", target),
            OR(target) => write!(f, "OR {:?}", target),
            CP(target) => write!(f, "CP {:?}", target),
            RET(cond) => write!(f, "RET {:?}", cond),
            LDHL(value) => write!(f, "LDHL {:?}", value),
            POP(pair) => write!(f, "POP {:?}", pair),
            RETI => f.write_str("RETI"),
            JP(cond, target) => write!(f, "JP {:?}, {:?}", cond, target),
            DI => f.write_str("DI"),
            EI => f.write_str("EI"),
            CALL(cond, addr) => write!(f, "CALL {:?}, {:?}", cond, addr),
            PUSH(pair) => write!(f, "PUSH {:?}", pair),
            RST(vector) => write!(f, "RST {:?}", vector),
            RLC(register) => write!(f, "RLC {:?}", register),
            RRC(register) => write!(f, "RRC {:?}", register),
            RL(register) => write!(f, "RL {:?}", register),
            RR(register) => write!(f, "RR {:?}", register),
            SLA(register) => write!(f, "SLA {:?}", register),
            SRA(register) => write!(f, "SRA {:?}", register),
            SWAP(register) => write!(f, "SWAP {:?}", register),
            SRL(register) => write!(f, "SRL {:?}", register),
            BIT(bit, register) => write!(f, "BIT {:?}, {:?}", bit, register),
            RES(bit, register) => write!(f, "RES {:?}, {:?}", bit, register),
            SET(bit, register) => write!(f, "SET {:?}, {:?}", bit, register),
        }
    }
}

impl std::fmt::Debug for JumpCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use JumpCondition::*;

        match *self {
            NotZero => f.write_str("NZ"),
            Zero => f.write_str("Z"),
            NotCarry => f.write_str("NC"),
            Carry => f.write_str("C"),
            Always => f.write_str(""),
        }
    }
}

impl std::fmt::Debug for InstrRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstrRegister::*;

        match *self {
            A => f.write_str("A"),
            B => f.write_str("B"),
            C => f.write_str("C"),
            D => f.write_str("D"),
            E => f.write_str("E"),
            H => f.write_str("H"),
            L => f.write_str("L"),
            IndirectHL => f.write_str("(HL)"),
        }
    }
}

impl Cycle {
    pub const fn new(num: u32) -> Self {
        Self(num)
    }
}

impl std::ops::Add for Cycle {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl std::ops::Add<u32> for Cycle {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl std::ops::AddAssign for Cycle {
    fn add_assign(&mut self, rhs: Self) {
        *self = Self(self.0 + rhs.0);
    }
}

impl std::ops::AddAssign<u32> for Cycle {
    fn add_assign(&mut self, rhs: u32) {
        *self = Self(self.0 + rhs);
    }
}

impl std::ops::Rem for Cycle {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self(self.0 % rhs.0)
    }
}

impl std::ops::Rem<u32> for Cycle {
    type Output = Self;

    fn rem(self, rhs: u32) -> Self::Output {
        Self(self.0 % rhs)
    }
}

impl std::ops::RemAssign for Cycle {
    fn rem_assign(&mut self, rhs: Self) {
        *self = Self(self.0 % rhs.0);
    }
}

impl std::ops::RemAssign<u32> for Cycle {
    fn rem_assign(&mut self, rhs: u32) {
        *self = Self(self.0 % rhs);
    }
}

impl std::ops::Sub for Cycle {
    type Output = Cycle;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl std::ops::Sub<u32> for Cycle {
    type Output = Cycle;

    fn sub(self, rhs: u32) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl std::ops::SubAssign for Cycle {
    fn sub_assign(&mut self, rhs: Self) {
        *self = Self(self.0 - rhs.0);
    }
}

impl std::ops::SubAssign<u32> for Cycle {
    fn sub_assign(&mut self, rhs: u32) {
        *self = Self(self.0 - rhs);
    }
}

impl PartialEq<u32> for Cycle {
    fn eq(&self, other: &u32) -> bool {
        self.0 == *other
    }
}

impl std::ops::Div for Cycle {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(self.0 / rhs.0)
    }
}

impl std::ops::Div<u32> for Cycle {
    type Output = Self;

    fn div(self, rhs: u32) -> Self::Output {
        Self::new(self.0 / rhs)
    }
}

impl From<u32> for Cycle {
    fn from(num: u32) -> Self {
        Self(num)
    }
}

impl From<Cycle> for u32 {
    fn from(cycles: Cycle) -> Self {
        cycles.0
    }
}

impl InstrRegisterPair {
    fn to_register_pair(self) -> RegisterPair {
        RegisterPair::try_from(self).expect("InstrRegisterPair is a valid RegisterPair")
    }
}

impl InstrRegister {
    fn to_register(self) -> Register {
        Register::try_from(self).expect("InstrRegister is a valid Register")
    }
}

#[cfg(test)]
mod tests {
    use super::Cycle;

    #[test]
    fn cycle_add_works() {
        let lhs: Cycle = Cycle::new(5);
        let rhs: Cycle = Cycle::new(4);

        assert_eq!(Cycle::new(9), rhs + lhs);
    }

    #[test]
    fn cycle_add_assign_works() {
        let mut cycles: Cycle = Cycle::new(5);
        cycles += 5;

        assert_eq!(Cycle::new(10), cycles);
    }
}
