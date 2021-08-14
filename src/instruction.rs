use self::add::{Source as AddSource, Target as AddTarget};
use self::alu::Source as AluSource;
use self::cycle::Cycle;
use self::jump::{JumpCondition, JumpLocation};
use self::load::{Source as LDSource, Target as LDTarget};
use self::table::{
    alu_imm_instr, alu_reg_instr, flag_instr, group1, group2, group3, jump_cond, prefix_alu,
    register,
};
use self::table::{Group1RegisterPair, Group2RegisterPair, Group3RegisterPair, Register};
use crate::bus::{Bus, BusIo};
use crate::cpu::{Cpu, Flags, HaltKind, ImeState, Register as CpuRegister, RegisterPair};

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy)]
pub(crate) enum Instruction {
    NOP,
    STOP,
    JR(JumpCondition),
    LD(LDTarget, LDSource),
    ADD(AddTarget, AddSource),
    LDHL,
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
    ADC(AluSource),
    SUB(AluSource),
    SBC(AluSource),
    AND(AluSource),
    XOR(AluSource),
    OR(AluSource),
    CP(AluSource),
    RET(JumpCondition),
    POP(Group3RegisterPair),
    RETI,
    JP(JumpCondition, JumpLocation),
    DI,
    EI,
    CALL(JumpCondition),
    PUSH(Group3RegisterPair),
    RST(u8),
    RLC(Register),
    RRC(Register),
    RL(Register),
    RR(Register),
    SLA(Register),
    SRA(Register),
    SWAP(Register),
    SRL(Register),
    BIT(u8, Register),
    RES(u8, Register),
    SET(u8, Register),
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;

        match self {
            NOP => f.write_str("NOP"),
            STOP => f.write_str("STOP"),
            JR(c) => write!(f, "JR {:?} i8", c),
            LD(t, s) => write!(f, "LD {:?}, {:?}", t, s),
            ADD(t, s) => write!(f, "ADD {:?}, {:?}", t, s),
            LDHL => f.write_str("LD HL, SP + i8"),
            INC(rs) => write!(f, "INC {:?}", rs),
            DEC(rs) => write!(f, "DEC {:?}", rs),
            RLCA => f.write_str("RLCA"),
            RRCA => f.write_str("RRCA"),
            RLA => f.write_str("RLA"),
            RRA => f.write_str("RRA"),
            DAA => f.write_str("DAA"),
            CPL => f.write_str("CPL"),
            SCF => f.write_str("SCF"),
            CCF => f.write_str("CCF"),
            HALT => f.write_str("HALT"),
            ADC(s) => write!(f, "ADC {:?}", s),
            SUB(s) => write!(f, "SUB {:?}", s),
            SBC(s) => write!(f, "SBC {:?}", s),
            AND(s) => write!(f, "AND {:?}", s),
            XOR(s) => write!(f, "XOR {:?}", s),
            OR(s) => write!(f, "OR {:?}", s),
            CP(s) => write!(f, "CP {:?}", s),
            RET(c) => write!(f, "RET {:?}", c),
            POP(rp) => write!(f, "POP: {:?}", rp),
            RETI => f.write_str("RETI"),
            JP(c, l) => write!(f, "JP {:?} {:?}", c, l),
            DI => f.write_str("DI"),
            EI => f.write_str("EI"),
            CALL(c) => write!(f, "CALL {:?}", c),
            PUSH(rp) => write!(f, "PUSH {:?}", rp),
            RST(v) => write!(f, "RST {:#04X}", v),
            RLC(r) => write!(f, "RLC {:?}", r),
            RRC(r) => write!(f, "RRC {:?}", r),
            RL(r) => write!(f, "RL {:?}", r),
            RR(r) => write!(f, "RR {:?}", r),
            SLA(r) => write!(f, "SLA {:?}", r),
            SRA(r) => write!(f, "SRA {:?}", r),
            SWAP(r) => write!(f, "SWAP {:?}", r),
            SRL(r) => write!(f, "SRL {:?}", r),
            BIT(b, r) => write!(f, "BIT {}, {:?}", b, r),
            RES(b, r) => write!(f, "RES {}, {:?}", b, r),
            SET(b, r) => write!(f, "SET {}, {:?}", b, r),
        }
    }
}

impl Instruction {
    pub(crate) fn execute(cpu: &mut Cpu, instruction: Self) -> Cycle {
        match instruction {
            Instruction::NOP => Cycle::new(4),
            Instruction::LD(target, src) => match (target, src) {
                (LDTarget::IndirectImmediateWord, LDSource::SP) => {
                    // LD (u16), SP | Store stack pointer in byte at 16-bit register
                    let addr = Self::imm_word(cpu);
                    let sp = cpu.register_pair(RegisterPair::SP);
                    Self::write_word(&mut cpu.bus, addr, sp);
                    Cycle::new(20)
                }
                (LDTarget::Group1(pair), LDSource::ImmediateWord) => {
                    // LD r16, u16 | Store u16 in 16-bit register
                    use Group1RegisterPair::*;
                    let word = Self::imm_word(cpu);

                    match pair {
                        BC | DE | HL | SP => cpu.set_register_pair(pair.as_register_pair(), word),
                    }
                    Cycle::new(12)
                }
                (LDTarget::IndirectGroup2(pair), LDSource::A) => {
                    // LD (r16), A | Store accumulator in byte at 16-bit register
                    let acc = cpu.register(CpuRegister::A);

                    match pair {
                        Group2RegisterPair::BC | Group2RegisterPair::DE => {
                            let addr = cpu.register_pair(pair.as_register_pair());
                            Self::write_byte(&mut cpu.bus, addr, acc);
                        }
                        Group2RegisterPair::IncrementHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            Self::write_byte(&mut cpu.bus, addr, acc);
                            cpu.set_register_pair(RegisterPair::HL, addr + 1);
                        }
                        Group2RegisterPair::DecrementHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            Self::write_byte(&mut cpu.bus, addr, acc);
                            cpu.set_register_pair(RegisterPair::HL, addr - 1);
                        }
                    }
                    Cycle::new(8)
                }
                (LDTarget::A, LDSource::IndirectGroup2(pair)) => {
                    // LD A, (r16) | Store byte at 16-bit register in accumulator
                    match pair {
                        Group2RegisterPair::BC | Group2RegisterPair::DE => {
                            let addr = cpu.register_pair(pair.as_register_pair());
                            let byte = Self::read_byte(&mut cpu.bus, addr);
                            cpu.set_register(CpuRegister::A, byte);
                        }
                        Group2RegisterPair::IncrementHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let byte = Self::read_byte(&mut cpu.bus, addr);
                            cpu.set_register(CpuRegister::A, byte);
                            cpu.set_register_pair(RegisterPair::HL, addr + 1);
                        }
                        Group2RegisterPair::DecrementHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let byte = Self::read_byte(&mut cpu.bus, addr);
                            cpu.set_register(CpuRegister::A, byte);
                            cpu.set_register_pair(RegisterPair::HL, addr - 1);
                        }
                    }
                    Cycle::new(8)
                }
                (LDTarget::Register(reg), LDSource::ImmediateByte) => {
                    // LD r8, u8 | Store u8 in 8-bit register
                    use Register::*;
                    let right = Self::imm_byte(cpu);

                    match reg {
                        A | B | C | D | E | H | L => {
                            cpu.set_register(reg.cpu_register(), right);
                            Cycle::new(8)
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            Self::write_byte(&mut cpu.bus, addr, right);
                            Cycle::new(12)
                        }
                    }
                }
                (LDTarget::IoWithC, LDSource::A) => {
                    // LD (0xFF00 + C), A | Store accumulator in byte at address 0xFF00 + C
                    let addr = 0xFF00 + cpu.register(CpuRegister::C) as u16;
                    let acc = cpu.register(CpuRegister::A);
                    Self::write_byte(&mut cpu.bus, addr, acc);
                    Cycle::new(8)
                }
                (LDTarget::A, LDSource::IoWithC) => {
                    // LD A, (0xFF00 + C) | Store byte at 0xFF00 + C in register A
                    let addr = 0xFF00 + cpu.register(CpuRegister::C) as u16;
                    let byte = Self::read_byte(&mut cpu.bus, addr);
                    cpu.set_register(CpuRegister::A, byte);
                    Cycle::new(8)
                }
                (LDTarget::Register(target), LDSource::Register(source)) => {
                    // LD r8, r8 | Store 8-bit register in 8-bit register
                    use Register::*;

                    match source {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(source.cpu_register());

                            match target {
                                B | C | D | E | H | L | A => {
                                    cpu.set_register(target.cpu_register(), right);
                                    Cycle::new(4)
                                }
                                IndirectHL => {
                                    let addr = cpu.register_pair(RegisterPair::HL);
                                    Self::write_byte(&mut cpu.bus, addr, right);
                                    Cycle::new(8)
                                }
                            }
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);

                            match target {
                                B | C | D | E | H | L | A => {
                                    cpu.set_register(target.cpu_register(), right);
                                    Cycle::new(8)
                                }
                                IndirectHL => {
                                    unreachable!("LD (HL), (HL) is an illegal instruction")
                                }
                            }
                        }
                    }
                }
                (LDTarget::IoWithImmediateOffset, LDSource::A) => {
                    // LD (0xFF00 + u8), A | Store accumulator in byte at address 0xFF00 + u8
                    let addr = 0xFF00 + Self::imm_byte(cpu) as u16;
                    let acc = cpu.register(CpuRegister::A);
                    Self::write_byte(&mut cpu.bus, addr, acc);
                    Cycle::new(12)
                }
                (LDTarget::A, LDSource::IoWithImmediateOffset) => {
                    // LD A, (0xFF00 + u8) | Store byte at address 0xFF00 + u8 in accumulator
                    let addr = 0xFF00 + Self::imm_byte(cpu) as u16;
                    let byte = Self::read_byte(&mut cpu.bus, addr);
                    cpu.set_register(CpuRegister::A, byte);
                    Cycle::new(12)
                }
                (LDTarget::SP, LDSource::HL) => {
                    // LD SP, HL | Store HL in stack pointer
                    cpu.set_register_pair(RegisterPair::SP, cpu.register_pair(RegisterPair::HL));
                    Cycle::new(8) // performs an internal operation that takes 4 cycles
                }
                (LDTarget::IndirectImmediateWord, LDSource::A) => {
                    // LD (u16), A | Store accumulator in byte at 16-bit register
                    let addr = Self::imm_word(cpu);
                    let acc = cpu.register(CpuRegister::A);
                    Self::write_byte(&mut cpu.bus, addr, acc);
                    Cycle::new(16)
                }
                (LDTarget::A, LDSource::IndirectImmediateWord) => {
                    // LD A, (u16) | Store byte at 16-bit register in accumulator
                    let addr = Self::imm_word(cpu);
                    let byte = Self::read_byte(&mut cpu.bus, addr);
                    cpu.set_register(CpuRegister::A, byte);
                    Cycle::new(16)
                }
                _ => unreachable!("LD {:?}, {:?} is an illegal instruction", target, src),
            },
            Instruction::STOP => todo!("SM83 Instruction STOP executed"),
            Instruction::JR(cond) => {
                // JR cond i8 | Add i8 bytes from program counter if condition is true
                // JR i8      | Add i8 bytes from program counter
                let flags: Flags = *cpu.flags();

                let byte = Self::imm_byte(cpu) as i8; // Note: This modifies the PC we access immediately after
                let pc = cpu.register_pair(RegisterPair::PC);
                let addr = pc.wrapping_add(byte as u16);

                match cond {
                    JumpCondition::NotZero => {
                        if !flags.z() {
                            Self::jump(cpu, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Zero => {
                        if flags.z() {
                            Self::jump(cpu, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c() {
                            Self::jump(cpu, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Carry => {
                        if flags.c() {
                            Self::jump(cpu, addr);
                            Cycle::new(12)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Always => {
                        Self::jump(cpu, addr);
                        Cycle::new(12)
                    }
                }
            }
            Instruction::ADD(target, src) => match (target, src) {
                (AddTarget::HL, AddSource::Group1(pair)) => {
                    // ADD HL, r16 | Add 16-bit register to HL
                    // FIXME: Memory Timings are not properly emulated for this instruction
                    use Group1RegisterPair::*;
                    let mut flags: Flags = *cpu.flags();

                    match pair {
                        BC | DE | HL | SP => {
                            let left = cpu.register_pair(RegisterPair::HL);
                            let right = cpu.register_pair(pair.as_register_pair());
                            cpu.set_register_pair(
                                RegisterPair::HL,
                                Self::add_u16(left, right, &mut flags),
                            );
                        }
                    }
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
                (AddTarget::A, AddSource::Register(reg)) => {
                    // ADD A, r8 | Add 8-bit register to accumulator
                    use Register::*;
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);

                    let (cycles, sum) = match reg {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(reg.cpu_register());
                            (Cycle::new(4), Self::add(left, right, &mut flags))
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            (Cycle::new(8), Self::add(left, right, &mut flags))
                        }
                    };

                    cpu.set_register(CpuRegister::A, sum);
                    cpu.set_flags(flags);
                    cycles
                }
                (AddTarget::SP, AddSource::ImmediateSignedByte) => {
                    // ADD SP, i8 | Add i8 to stack pointer
                    // FIXME: Memory Timings are not properly emulated for this instruction
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register_pair(RegisterPair::SP);
                    let sum = Self::add_u16_i8(left, Self::imm_byte(cpu) as i8, &mut flags);
                    cpu.set_register_pair(RegisterPair::SP, sum);
                    cpu.set_flags(flags);
                    Cycle::new(16)
                }
                (AddTarget::A, AddSource::ImmediateByte) => {
                    // ADD A, u8 | Add u8 to accumulator
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);
                    let sum = Self::add(left, Self::imm_byte(cpu), &mut flags);
                    cpu.set_register(CpuRegister::A, sum);
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
                _ => unreachable!("ADD {:?}, {:?} is an illegal instruction", target, src),
            },
            Instruction::INC(registers) => {
                match registers {
                    AllRegisters::Register(reg) => {
                        // INC r8 | Increment 8-bit register
                        use Register::*;
                        let mut flags: Flags = *cpu.flags();

                        let cycles = match reg {
                            B | C | D | E | H | L | A => {
                                let reg = reg.cpu_register();
                                cpu.set_register(reg, Self::inc(cpu.register(reg), &mut flags));
                                Cycle::new(4)
                            }
                            IndirectHL => {
                                let addr = cpu.register_pair(RegisterPair::HL);
                                let left = Self::read_byte(&mut cpu.bus, addr);
                                Self::write_byte(&mut cpu.bus, addr, Self::inc(left, &mut flags));
                                Cycle::new(12)
                            }
                        };
                        cpu.set_flags(flags);
                        cycles
                    }
                    AllRegisters::Group1(pair) => {
                        // INC r16 | Increment 16-bit register
                        // Note: No flags are set with this version of the INC instruction
                        // FIXME: Memory Timings are not properly emulated for this instruction
                        use Group1RegisterPair::*;

                        match pair {
                            BC | DE | HL | SP => {
                                let pair = pair.as_register_pair();
                                let left = cpu.register_pair(pair);
                                cpu.set_register_pair(pair, left.wrapping_add(1));
                            }
                        }
                        Cycle::new(8)
                    }
                }
            }
            Instruction::DEC(registers) => {
                match registers {
                    AllRegisters::Register(reg) => {
                        // DEC r8 | Decrement 8-bit register
                        use Register::*;
                        let mut flags: Flags = *cpu.flags();

                        let cycles = match reg {
                            B | C | D | E | H | L | A => {
                                let reg = reg.cpu_register();
                                cpu.set_register(reg, Self::dec(cpu.register(reg), &mut flags));
                                Cycle::new(4)
                            }
                            IndirectHL => {
                                let addr = cpu.register_pair(RegisterPair::HL);
                                let left = Self::read_byte(&mut cpu.bus, addr);
                                Self::write_byte(&mut cpu.bus, addr, Self::dec(left, &mut flags));
                                Cycle::new(12)
                            }
                        };
                        cpu.set_flags(flags);
                        cycles
                    }
                    AllRegisters::Group1(pair) => {
                        // DEC r16 | Decrement Register Pair
                        // FIXME: Memory Timings are not properly emulated for this instruction
                        use Group1RegisterPair::*;

                        match pair {
                            BC | DE | HL | SP => {
                                let pair = pair.as_register_pair();
                                let left = cpu.register_pair(pair);
                                cpu.set_register_pair(pair, left.wrapping_sub(1));
                            }
                        };
                        Cycle::new(8)
                    }
                }
            }
            Instruction::RLCA => {
                // RLCA | Rotate accumulator left
                let acc = cpu.register(CpuRegister::A);
                let most_sgfnt = acc >> 7;
                let acc_rotated = acc.rotate_left(1);
                cpu.set_register(CpuRegister::A, acc_rotated);
                cpu.update_flags(false, false, false, most_sgfnt == 0x01);
                Cycle::new(4)
            }
            Instruction::RRCA => {
                // RRCA | Rotate accumulator right
                let acc = cpu.register(CpuRegister::A);
                let least_sgfnt = acc & 0x01;
                let acc_rotated = acc.rotate_right(1);
                cpu.set_register(CpuRegister::A, acc_rotated);
                cpu.update_flags(false, false, false, least_sgfnt == 0x01);
                Cycle::new(4)
            }
            Instruction::RLA => {
                // RLA | Rotate accumulator left through carry
                let flags: Flags = *cpu.flags();

                let acc = cpu.register(CpuRegister::A);
                let (acc_rotated, carry) = Self::rl_thru_carry(acc, flags.c());
                cpu.set_register(CpuRegister::A, acc_rotated);
                cpu.update_flags(false, false, false, carry);
                Cycle::new(4)
            }
            Instruction::RRA => {
                // RRA | Rotate accumulator right through carry
                let flags: Flags = *cpu.flags();

                let acc = cpu.register(CpuRegister::A);
                let (acc_rotated, carry) = Self::rr_thru_carry(acc, flags.c());
                cpu.set_register(CpuRegister::A, acc_rotated);
                cpu.update_flags(false, false, false, carry);
                Cycle::new(4)
            }
            Instruction::DAA => {
                // DAA | Change accumulator into its BCD representation
                // resource: https://ehaskins.com/2018-01-30%20Z80%20DAA/
                // https://github.com/mamedev/mame/blob/master/src/devices/cpu/lr35902/opc_main.hxx#L354
                let mut flags = *cpu.flags();
                let mut tmp = cpu.register(CpuRegister::A) as i16;

                if !flags.n() {
                    // Positive
                    if flags.h() || tmp & 0x0F > 0x09 {
                        tmp += 0x06;
                    }

                    if flags.c() || tmp > 0x9F {
                        tmp += 0x60;
                    }
                } else {
                    // Negative
                    if flags.h() {
                        tmp -= 6;

                        if !flags.c() {
                            tmp &= 0xFF;
                        }
                    }

                    if flags.c() {
                        tmp -= 0x60;
                    }
                }

                if tmp & 0x100 != 0 {
                    flags.set_c(true);
                }

                cpu.set_register(CpuRegister::A, tmp as u8);
                flags.set_z(tmp as u8 == 0);
                flags.set_h(false);
                cpu.set_flags(flags);
                Cycle::new(4)
            }
            Instruction::CPL => {
                // CPL | Compliment accumulator
                let mut flags: Flags = *cpu.flags();

                let acc = cpu.register(CpuRegister::A);
                cpu.set_register(CpuRegister::A, !acc); // Bitwise not is ! instead of ~
                flags.set_n(true);
                flags.set_h(true);
                cpu.set_flags(flags);
                Cycle::new(4)
            }
            Instruction::SCF => {
                // SCF | Set Carry Flag
                let mut flags: Flags = *cpu.flags();

                flags.set_n(false);
                flags.set_h(false);
                flags.set_c(true);
                cpu.set_flags(flags);
                Cycle::new(4)
            }
            Instruction::CCF => {
                // CCF | Compliment Carry Flag
                let mut flags: Flags = *cpu.flags();

                flags.set_n(false);
                flags.set_h(false);
                flags.set_c(!flags.c());
                cpu.set_flags(flags);
                Cycle::new(4)
            }
            Instruction::HALT => {
                // HALT | Enter CPU low power consumption mode until interrupt occurs
                use HaltKind::*;

                let kind = match *cpu.ime() {
                    ImeState::Enabled => ImeEnabled,
                    _ if cpu.int_request() & cpu.int_enable() != 0 => SomePending,
                    _ => NonePending,
                };
                cpu.halt(kind);
                Cycle::new(4)
            }
            Instruction::ADC(source) => match source {
                AluSource::Register(reg) => {
                    // ADC A, r8 | Add 8-bit register to accumulator with carry
                    use Register::*;
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);

                    let (cycles, sum) = match reg {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(reg.cpu_register());
                            let sum = Self::add_with_carry_bit(left, right, flags.c(), &mut flags);
                            (Cycle::new(4), sum)
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            let sum = Self::add_with_carry_bit(left, right, flags.c(), &mut flags);
                            (Cycle::new(8), sum)
                        }
                    };
                    cpu.set_register(CpuRegister::A, sum);
                    cpu.set_flags(flags);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // ADC A, u8 | Add u8 to accumulator with carry
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);
                    let right = Self::imm_byte(cpu);
                    let sum = Self::add_with_carry_bit(left, right, flags.c(), &mut flags);
                    cpu.set_register(CpuRegister::A, sum);
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
            },
            Instruction::SUB(source) => match source {
                AluSource::Register(reg) => {
                    // SUB r8 | Subtract 8-bit register from accumulator
                    use Register::*;
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);

                    let (cycles, diff) = match reg {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(reg.cpu_register());
                            (Cycle::new(4), Self::sub(left, right, &mut flags))
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            (Cycle::new(8), Self::sub(left, right, &mut flags))
                        }
                    };
                    cpu.set_register(CpuRegister::A, diff);
                    cpu.set_flags(flags);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // SUB u8 | Subtract u8 from accumulator
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);
                    let right = Self::imm_byte(cpu);
                    cpu.set_register(CpuRegister::A, Self::sub(left, right, &mut flags));
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
            },
            Instruction::SBC(target) => match target {
                AluSource::Register(reg) => {
                    // SBC r8 | Subtract 8-bit register from accumulator with carry
                    use Register::*;
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);

                    let (cycles, diff) = match reg {
                        B | C | D | E | H | L | A => {
                            let right = cpu.register(reg.cpu_register());
                            let diff = Self::sub_with_carry(left, right, flags.c(), &mut flags);
                            (Cycle::new(4), diff)
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            let diff = Self::sub_with_carry(left, right, flags.c(), &mut flags);
                            (Cycle::new(8), diff)
                        }
                    };
                    cpu.set_register(CpuRegister::A, diff);
                    cpu.set_flags(flags);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // SBC u8 | Subtract u8 from accumulator with carry
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);
                    let right = Self::imm_byte(cpu);
                    let diff = Self::sub_with_carry(left, right, flags.c(), &mut flags);
                    cpu.set_register(CpuRegister::A, diff);
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
            },
            Instruction::AND(target) => match target {
                AluSource::Register(reg) => {
                    // AND r8 | Perform bitwise AND on accumulator and 8-bit register
                    use Register::*;
                    let left = cpu.register(CpuRegister::A);

                    let (cycles, acc) = match reg {
                        B | C | D | E | H | L | A => {
                            (Cycle::new(4), left & cpu.register(reg.cpu_register()))
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            (Cycle::new(8), left & right)
                        }
                    };
                    cpu.set_register(CpuRegister::A, acc);
                    cpu.update_flags(acc == 0, false, true, false);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // AND u8 | Perform bitwise AND on accumulator and u8
                    let acc = cpu.register(CpuRegister::A) & Self::imm_byte(cpu);
                    cpu.set_register(CpuRegister::A, acc);
                    cpu.update_flags(acc == 0, false, true, false);
                    Cycle::new(8)
                }
            },
            Instruction::XOR(source) => match source {
                AluSource::Register(reg) => {
                    // XOR r8 | Perform bitwise XOR on accumulator and 8-bit register
                    use Register::*;
                    let left = cpu.register(CpuRegister::A);

                    let (cycles, acc) = match reg {
                        B | C | D | E | H | L | A => {
                            (Cycle::new(4), left ^ cpu.register(reg.cpu_register()))
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            (Cycle::new(8), left ^ right)
                        }
                    };
                    cpu.set_register(CpuRegister::A, acc);
                    cpu.update_flags(acc == 0, false, false, false);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // XOR u8 | Perform bitwise XOR on accumulator and u8
                    let acc = cpu.register(CpuRegister::A) ^ Self::imm_byte(cpu);
                    cpu.set_register(CpuRegister::A, acc);
                    cpu.update_flags(acc == 0, false, false, false);
                    Cycle::new(8)
                }
            },
            Instruction::OR(target) => match target {
                AluSource::Register(reg) => {
                    // OR r8 | Perform bitwise OR on accumulator and 8-bit register
                    use Register::*;
                    let left = cpu.register(CpuRegister::A);

                    let (cycles, acc) = match reg {
                        B | C | D | E | H | L | A => {
                            (Cycle::new(4), left | cpu.register(reg.cpu_register()))
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            (Cycle::new(8), left | right)
                        }
                    };
                    cpu.set_register(CpuRegister::A, acc);
                    cpu.update_flags(acc == 0, false, false, false);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // OR u8 | Perform bitwise OR on accumulator and u8
                    let acc = cpu.register(CpuRegister::A) | Self::imm_byte(cpu);
                    cpu.set_register(CpuRegister::A, acc);
                    cpu.update_flags(acc == 0, false, false, false);
                    Cycle::new(8)
                }
            },
            Instruction::CP(target) => match target {
                AluSource::Register(reg) => {
                    // CP r8 | Compare accumulator to 8-bit register. Do not store result
                    use Register::*;
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);

                    let cycles = match reg {
                        B | C | D | E | H | L | A => {
                            let _ = Self::sub(left, cpu.register(reg.cpu_register()), &mut flags);
                            Cycle::new(4)
                        }
                        IndirectHL => {
                            let addr = cpu.register_pair(RegisterPair::HL);
                            let right = Self::read_byte(&mut cpu.bus, addr);
                            let _ = Self::sub(left, right, &mut flags);
                            Cycle::new(8)
                        }
                    };
                    cpu.set_flags(flags);
                    cycles
                }
                AluSource::ImmediateByte => {
                    // CP u8 | Compare accumulator to u8. Do not store result
                    let mut flags: Flags = *cpu.flags();

                    let left = cpu.register(CpuRegister::A);
                    let _ = Self::sub(left, Self::imm_byte(cpu), &mut flags);
                    cpu.set_flags(flags);
                    Cycle::new(8)
                }
            },
            Instruction::LDHL => {
                // LD HL, SP + i8 | Store stack pointer + i8 in HL
                let mut flags: Flags = *cpu.flags();

                let left = cpu.register_pair(RegisterPair::SP);
                let sum = Self::add_u16_i8(left, Self::imm_byte(cpu) as i8, &mut flags);
                cpu.set_register_pair(RegisterPair::HL, sum);
                cpu.set_flags(flags);
                cpu.bus.clock(); // FIXME: Is this in the right place?
                Cycle::new(12)
            }
            Instruction::RET(cond) => {
                // RET cond | Return from subroutine if condition is true
                // RET      | Return from subroutine
                let flags: Flags = *cpu.flags();

                match cond {
                    JumpCondition::NotZero => {
                        cpu.bus.clock(); // internal branch decision

                        if !flags.z() {
                            let addr = Self::pop(cpu);
                            Self::jump(cpu, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Zero => {
                        cpu.bus.clock(); // internal branch decision

                        if flags.z() {
                            let addr = Self::pop(cpu);
                            Self::jump(cpu, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::NotCarry => {
                        cpu.bus.clock(); // internal branch decision

                        if !flags.c() {
                            let addr = Self::pop(cpu);
                            Self::jump(cpu, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Carry => {
                        cpu.bus.clock(); // internal branch decision

                        if flags.c() {
                            let addr = Self::pop(cpu);
                            Self::jump(cpu, addr);
                            Cycle::new(20)
                        } else {
                            Cycle::new(8)
                        }
                    }
                    JumpCondition::Always => {
                        let addr = Self::pop(cpu);
                        Self::jump(cpu, addr);
                        Cycle::new(16)
                    }
                }
            }
            Instruction::POP(pair) => {
                // POP r16 | Store word popped from the stack in r16
                use Group3RegisterPair::*;

                match pair {
                    BC | DE | HL | AF => {
                        let right = Self::pop(cpu);
                        cpu.set_register_pair(pair.as_register_pair(), right);
                    }
                }
                Cycle::new(12)
            }
            Instruction::RETI => {
                // RETI | Return from subroutine, then enable interrupts
                let addr = Self::pop(cpu);
                Self::jump(cpu, addr);
                cpu.set_ime(ImeState::Enabled);
                Cycle::new(16)
            }
            Instruction::JP(cond, location) => match location {
                JumpLocation::HL => {
                    // JP HL | Store HL in program counter
                    let right = cpu.register_pair(RegisterPair::HL);
                    cpu.set_register_pair(RegisterPair::PC, right);
                    Cycle::new(4)
                }
                JumpLocation::ImmediateWord => {
                    // JP cond u16 | Store u16 in program counter if condition is true
                    // JP u16      | Store u16 in program counter
                    let flags: Flags = *cpu.flags();

                    let addr = Self::imm_word(cpu);

                    match cond {
                        JumpCondition::NotZero => {
                            if !flags.z() {
                                Self::jump(cpu, addr);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::Zero => {
                            if flags.z() {
                                Self::jump(cpu, addr);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::NotCarry => {
                            if !flags.c() {
                                Self::jump(cpu, addr);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::Carry => {
                            if flags.c() {
                                Self::jump(cpu, addr);
                                Cycle::new(16)
                            } else {
                                Cycle::new(12)
                            }
                        }
                        JumpCondition::Always => {
                            Self::jump(cpu, addr);
                            Cycle::new(16)
                        }
                    }
                }
            },
            Instruction::DI => {
                // DI | Disable IME
                cpu.set_ime(ImeState::Disabled);
                Cycle::new(4)
            }
            Instruction::EI => {
                // EI | Enable IME after the next instruction
                cpu.set_ime(ImeState::EiExecuted);
                Cycle::new(4)
            }
            Instruction::CALL(cond) => {
                // CALL cond u16 | Push PC on the stack and store u16 in program counter if condition is true
                // CALL u16      | Push PC on the stack then store u16 in program counter
                let flags: Flags = *cpu.flags();

                let addr = Self::imm_word(cpu);
                let return_addr = cpu.register_pair(RegisterPair::PC);

                match cond {
                    JumpCondition::NotZero => {
                        if !flags.z() {
                            cpu.bus.clock(); // internal branch decision
                            Self::push(cpu, return_addr);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::Zero => {
                        if flags.z() {
                            cpu.bus.clock(); // internal branch decision
                            Self::push(cpu, return_addr);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::NotCarry => {
                        if !flags.c() {
                            cpu.bus.clock(); // internal branch decision
                            Self::push(cpu, return_addr);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::Carry => {
                        if flags.c() {
                            cpu.bus.clock(); // internal branch decision
                            Self::push(cpu, return_addr);
                            cpu.set_register_pair(RegisterPair::PC, addr);
                            Cycle::new(24)
                        } else {
                            Cycle::new(12)
                        }
                    }
                    JumpCondition::Always => {
                        cpu.bus.clock(); // internal branch decision
                        Self::push(cpu, return_addr);
                        cpu.set_register_pair(RegisterPair::PC, addr);
                        Cycle::new(24)
                    }
                }
            }
            Instruction::PUSH(pair) => {
                // PUSH r16 | Push r16 onto the stack
                use Group3RegisterPair::*;

                cpu.bus.clock(); // internal

                match pair {
                    BC | DE | HL | AF => {
                        let word = cpu.register_pair(pair.as_register_pair());
                        Self::push(cpu, word);
                    }
                }
                Cycle::new(16)
            }
            Instruction::RST(vector) => {
                // RST vector | Push current address onto the stack, jump to 0x0000 + n
                Self::reset(cpu, vector)
            }
            Instruction::RLC(reg) => {
                // RLC r8 | Rotate r8 left
                use Register::*;

                let (cycles, most_sgfnt, rotated) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let rotated = byte.rotate_left(1);
                        cpu.set_register(reg, rotated);
                        (Cycle::new(8), byte >> 7, rotated)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let rotated = byte.rotate_left(1);
                        Self::write_byte(&mut cpu.bus, addr, rotated);
                        (Cycle::new(16), byte >> 7, rotated)
                    }
                };
                cpu.update_flags(rotated == 0, false, false, most_sgfnt == 0x01);
                cycles
            }
            Instruction::RRC(reg) => {
                // RRC r8 | Rotate r8 right
                use Register::*;

                let (cycles, least_sgfnt, rotated) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let rotated = byte.rotate_right(1);
                        cpu.set_register(reg, rotated);
                        (Cycle::new(8), byte & 0x01, rotated)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let rotated = byte.rotate_right(1);
                        Self::write_byte(&mut cpu.bus, addr, rotated);
                        (Cycle::new(16), byte & 0x01, rotated)
                    }
                };
                cpu.update_flags(rotated == 0, false, false, least_sgfnt == 0x01);
                cycles
            }
            Instruction::RL(reg) => {
                // RL r8 | Rotate r8 left through carry
                use Register::*;

                let flags: Flags = *cpu.flags();

                let (cycles, rotated, carry) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let (rotated, carry) = Self::rl_thru_carry(byte, flags.c());
                        cpu.set_register(reg, rotated);
                        (Cycle::new(8), rotated, carry)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let (rotated, carry) = Self::rl_thru_carry(byte, flags.c());
                        Self::write_byte(&mut cpu.bus, addr, rotated);
                        (Cycle::new(16), rotated, carry)
                    }
                };
                cpu.update_flags(rotated == 0, false, false, carry);
                cycles
            }
            Instruction::RR(reg) => {
                // RR r8 | Rotate register r8 right through carry
                use Register::*;

                let flags: Flags = *cpu.flags();

                let (cycles, rotated, carry) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let (rotated, carry) = Self::rr_thru_carry(byte, flags.c());
                        cpu.set_register(reg, rotated);
                        (Cycle::new(8), rotated, carry)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let (rotated, carry) = Self::rr_thru_carry(byte, flags.c());
                        Self::write_byte(&mut cpu.bus, addr, rotated);
                        (Cycle::new(16), rotated, carry)
                    }
                };
                cpu.update_flags(rotated == 0, false, false, carry);
                cycles
            }
            Instruction::SLA(reg) => {
                // SLA r8 | Shift left arithmetic r8
                use Register::*;

                let (cycles, most_sgfnt, shifted) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let shifted = byte << 1;
                        cpu.set_register(reg, shifted);
                        (Cycle::new(8), (byte >> 7) & 0x01, shifted)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let shifted = byte << 1;
                        Self::write_byte(&mut cpu.bus, addr, shifted);
                        (Cycle::new(16), (byte >> 7) & 0x01, shifted)
                    }
                };
                cpu.update_flags(shifted == 0, false, false, most_sgfnt == 0x01);
                cycles
            }
            Instruction::SRA(reg) => {
                // SRA r8 | Shift right arithmetic r8
                use Register::*;

                let (cycles, least_sgfnt, shifted) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let shifted = ((byte >> 7) & 0x01) << 7 | byte >> 1;
                        cpu.set_register(reg, shifted);
                        (Cycle::new(8), byte & 0x01, shifted)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let shifted = ((byte >> 7) & 0x01) << 7 | byte >> 1;
                        Self::write_byte(&mut cpu.bus, addr, shifted);
                        (Cycle::new(16), byte & 0x01, shifted)
                    }
                };
                cpu.update_flags(shifted == 0, false, false, least_sgfnt == 0x01);
                cycles
            }
            Instruction::SWAP(reg) => {
                // SWAP r[z] | Swap the two nybbles in a byte
                use Register::*;

                let (cycles, swapped) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let swapped = Self::swap_bits(cpu.register(reg));
                        cpu.set_register(reg, swapped);
                        (Cycle::new(8), swapped)
                    }

                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let swapped = Self::swap_bits(Self::read_byte(&mut cpu.bus, addr));
                        Self::write_byte(&mut cpu.bus, addr, swapped);
                        (Cycle::new(16), swapped)
                    }
                };
                cpu.update_flags(swapped == 0, false, false, false);
                cycles
            }
            Instruction::SRL(reg) => {
                // SRL r[z] | Shift right logic r8
                use Register::*;

                let (cycles, least_sgfnt, shift_reg) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        let shifted = byte >> 1;
                        cpu.set_register(reg, shifted);
                        (Cycle::new(8), byte & 0x01, shifted)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        let shifted = byte >> 1;
                        Self::write_byte(&mut cpu.bus, addr, shifted);
                        (Cycle::new(16), byte & 0x01, shifted)
                    }
                };
                cpu.update_flags(shift_reg == 0, false, false, least_sgfnt == 0x01);
                cycles
            }
            Instruction::BIT(bit, reg) => {
                // BIT u8, r8 | Test bit u8 in r8
                use Register::*;
                let mut flags: Flags = *cpu.flags();

                let (cycles, is_set) = match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        (Cycle::new(8), ((byte >> bit) & 0x01) == 0x01)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        (Cycle::new(12), ((byte >> bit) & 0x01) == 0x01)
                    }
                };
                flags.set_z(!is_set);
                flags.set_n(false);
                flags.set_h(true);
                cpu.set_flags(flags);
                cycles
            }
            Instruction::RES(bit, reg) => {
                // RES u8, r8 | Reset bit u8 in r8
                use Register::*;

                match reg {
                    B | C | D | E | H | L | A => {
                        let register = reg.cpu_register();
                        let byte = cpu.register(register);
                        cpu.set_register(register, byte & !(1 << bit));
                        Cycle::new(8)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        Self::write_byte(&mut cpu.bus, addr, byte & !(1 << bit));
                        Cycle::new(16)
                    }
                }
            }
            Instruction::SET(bit, reg) => {
                // SET u8, r8 | Set bit u8
                use Register::*;

                match reg {
                    B | C | D | E | H | L | A => {
                        let reg = reg.cpu_register();
                        let byte = cpu.register(reg);
                        cpu.set_register(reg, byte | (1u8 << bit));
                        Cycle::new(8)
                    }
                    IndirectHL => {
                        let addr = cpu.register_pair(RegisterPair::HL);
                        let byte = Self::read_byte(&mut cpu.bus, addr);
                        Self::write_byte(&mut cpu.bus, addr, byte | (1u8 << bit));
                        Cycle::new(16)
                    }
                }
            }
        }
    }

    /// PUSHes a u16 onto the stack
    ///
    /// Mutates the stack pointer and the stack (8 cycles)
    fn push(cpu: &mut Cpu, value: u16) {
        let mut sp = cpu.register_pair(RegisterPair::SP);

        sp -= 1;
        Self::write_byte(&mut cpu.bus, sp, (value >> 8) as u8);
        sp -= 1;
        Self::write_byte(&mut cpu.bus, sp, value as u8);

        cpu.set_register_pair(RegisterPair::SP, sp);
    }

    /// POPs a u16 from the stack
    ///
    /// Mutates the stack pointer and returns the u16 which was popped from the stack
    /// (8 cycles)
    fn pop(cpu: &mut Cpu) -> u16 {
        let mut sp = cpu.register_pair(RegisterPair::SP);

        let low = Self::read_byte(&mut cpu.bus, sp);
        sp += 1;
        let high = Self::read_byte(&mut cpu.bus, sp);
        sp += 1;

        cpu.set_register_pair(RegisterPair::SP, sp);
        (high as u16) << 8 | low as u16
    }

    fn dec(left: u8, flags: &mut Flags) -> u8 {
        Self::sub_no_carry(left, 1, flags)
    }

    fn inc(left: u8, flags: &mut Flags) -> u8 {
        Self::add_no_carry(left, 1, flags)
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
        cpu.bus.clock(); // internal
        let addr = cpu.register_pair(RegisterPair::PC);
        Self::push(cpu, addr);
        cpu.set_register_pair(RegisterPair::PC, vector as u16);
        Cycle::new(16)
    }

    /// Read u8 from memory (4 cycles)
    fn read_byte(bus: &mut Bus, addr: u16) -> u8 {
        let byte = bus.read_byte(addr);
        bus.clock();
        byte
    }

    /// Write u8 to memory (4 cycles)
    fn write_byte(bus: &mut Bus, addr: u16, byte: u8) {
        bus.write_byte(addr, byte);
        bus.clock();
    }

    /// Write u16 to memory (8 cycles)
    fn write_word(bus: &mut Bus, addr: u16, word: u16) {
        Self::write_byte(bus, addr, word as u8);
        Self::write_byte(bus, addr + 1, (word >> 8) as u8);
    }

    /// Read u16 from memory (8 cycles)
    fn read_word(bus: &mut Bus, addr: u16) -> u16 {
        // Must preserve the order, can't one-line this.
        let low = Self::read_byte(bus, addr);
        let high = Self::read_byte(bus, addr + 1);
        (high as u16) << 8 | low as u16
    }

    /// Fetch u16 from memory, increment the program counter by two
    /// (8 cycles)
    fn imm_word(cpu: &mut Cpu) -> u16 {
        let pc = cpu.register_pair(RegisterPair::PC);
        let word = Self::read_word(&mut cpu.bus, pc);
        cpu.set_register_pair(RegisterPair::PC, pc + 2);

        word
    }

    /// Fetch u8 from memory, increment the program counter by one
    /// (4 cycles)
    fn imm_byte(cpu: &mut Cpu) -> u8 {
        let pc = cpu.register_pair(RegisterPair::PC);
        let byte = Self::read_byte(&mut cpu.bus, pc);
        cpu.set_register_pair(RegisterPair::PC, pc + 1);

        byte
    }

    /// Set program counter to Address.
    ///
    /// This is explicitly meant to emulate the exact behaviour of JP, JR RET, RETI and CALL
    /// (4 cycles)
    fn jump(cpu: &mut Cpu, addr: u16) {
        cpu.set_register_pair(RegisterPair::PC, addr);
        cpu.bus.clock();
    }
}

impl Instruction {
    pub(crate) fn decode(byte: u8, prefixed: bool) -> Self {
        if prefixed {
            Self::prefixed(byte)
        } else {
            Self::unprefixed(byte)
        }
    }

    fn unprefixed(byte: u8) -> Self {
        use Instruction::*;

        match byte {
            // NOP
            0o000 => NOP,
            // LD (u16), SP
            0o010 => LD(LDTarget::IndirectImmediateWord, LDSource::SP),
            // STOP
            0o020 => STOP,
            // JR i8
            0o030 => JR(JumpCondition::Always),
            // JR cond i8
            0o040 | 0o050 | 0o060 | 0o070 => JR(jump_cond((byte >> 3) & 0x03)),
            // LD r16, u16
            0o001 | 0o021 | 0o041 | 0o061 => LD(
                LDTarget::Group1(group1((byte >> 4) & 0x03)),
                LDSource::ImmediateWord,
            ),
            // ADD HL, r16
            0o011 | 0o031 | 0o051 | 0o071 => {
                ADD(AddTarget::HL, AddSource::Group1(group1((byte >> 4) & 0x03)))
            }
            // LD (r16), A
            0o002 | 0o022 | 0o042 | 0o062 => LD(
                LDTarget::IndirectGroup2(group2((byte >> 4) & 0x03)),
                LDSource::A,
            ),
            // LD A, (r16)
            0o012 | 0o032 | 0o052 | 0o072 => LD(
                LDTarget::A,
                LDSource::IndirectGroup2(group2((byte >> 4) & 0x03)),
            ),
            // INC r16
            0o003 | 0o023 | 0o043 | 0o063 => INC(AllRegisters::Group1(group1((byte >> 4) & 0x03))),
            // DEC r16
            0o013 | 0o033 | 0o053 | 0o073 => DEC(AllRegisters::Group1(group1((byte >> 4) & 0x03))),
            // INC r8
            0o004 | 0o014 | 0o024 | 0o034 | 0o044 | 0o054 | 0o064 | 0o074 => {
                INC(AllRegisters::Register(register((byte >> 3) & 0x07)))
            }
            // DEC r8
            0o005 | 0o015 | 0o025 | 0o035 | 0o045 | 0o055 | 0o065 | 0o075 => {
                DEC(AllRegisters::Register(register((byte >> 3) & 0x07)))
            }
            // LD r8, u8
            0o006 | 0o016 | 0o026 | 0o036 | 0o046 | 0o056 | 0o066 | 0o076 => LD(
                LDTarget::Register(register((byte >> 3) & 0x07)),
                LDSource::ImmediateByte,
            ),
            // RLCA, RRCA, RLA, RRA, DAA, CPL, SCF, and CCF
            0o007 | 0o017 | 0o027 | 0o037 | 0o047 | 0o057 | 0o067 | 0o077 => {
                flag_instr((byte >> 3) & 0x07)
            }
            // HALT
            0o166 => HALT,
            // LD r8, r8
            0o100..=0o177 => LD(
                LDTarget::Register(register((byte >> 3) & 0x07)),
                LDSource::Register(register(byte & 0x07)),
            ),
            // ADD, ADC, SUB, SBC, AND, XOR, OR, and CP
            0o200..=0o277 => alu_reg_instr((byte >> 3) & 0x07, byte & 0x07),
            // RET cond
            0o300 | 0o310 | 0o320 | 0o330 => RET(jump_cond((byte >> 3) & 0x03)),
            // LD (0xFF00 + u8), A
            0o340 => LD(LDTarget::IoWithImmediateOffset, LDSource::A),
            // ADD SP, i8
            0o350 => ADD(AddTarget::SP, AddSource::ImmediateSignedByte),
            // LD A, (0xFF00 + u8)
            0o360 => LD(LDTarget::A, LDSource::IoWithImmediateOffset),
            // LD HL, SP + i8
            0o370 => LDHL,
            // POP r16
            0o301 | 0o321 | 0o341 | 0o361 => POP(group3((byte >> 4) & 0x03)),
            // RET
            0o311 => RET(JumpCondition::Always),
            // RETI
            0o331 => RETI,
            // JP HL
            0o351 => JP(JumpCondition::Always, JumpLocation::HL),
            // LD SP, HL
            0o371 => LD(LDTarget::SP, LDSource::HL),
            // JP cond u16
            0o302 | 0o312 | 0o322 | 0o332 => {
                JP(jump_cond((byte >> 3) & 0x03), JumpLocation::ImmediateWord)
            }
            // LD (0xFF00 + C), A
            0o342 => LD(LDTarget::IoWithC, LDSource::A),
            // LD (u16), A
            0o352 => LD(LDTarget::IndirectImmediateWord, LDSource::A),
            // LD A, (0xFF00 + C)
            0o362 => LD(LDTarget::A, LDSource::IoWithC),
            // LD A, (u16)
            0o372 => LD(LDTarget::A, LDSource::IndirectImmediateWord),
            // JP u16
            0o303 => JP(JumpCondition::Always, JumpLocation::ImmediateWord),
            // 0xCB Prefix
            0o313 => unreachable!("{:#04X} should be handled by the prefixed decoder", byte),
            // DI
            0o363 => DI,
            // EI
            0o373 => EI,
            // CALL cond u16
            0o304 | 0o314 | 0o324 | 0o334 => CALL(jump_cond((byte >> 3) & 0x03)),
            // PUSH r16
            0o305 | 0o325 | 0o345 | 0o365 => PUSH(group3((byte >> 4) & 0x03)),
            0o315 => CALL(JumpCondition::Always),
            0o306 | 0o316 | 0o326 | 0o336 | 0o346 | 0o356 | 0o366 | 0o376 => {
                alu_imm_instr((byte >> 3) & 0x07)
            }
            0o307 | 0o317 | 0o327 | 0o337 | 0o347 | 0o357 | 0o367 | 0o377 => RST(byte & 0b00111000),
            _ => panic!("{:#04X} is an illegal opcode", byte),
        }
    }

    fn prefixed(byte: u8) -> Self {
        use Instruction::*;

        match byte {
            // RLC, RRC, RL, RR, SLA, SRA, SWAP and SRL
            0o000..=0o077 => prefix_alu((byte >> 3) & 0x07, byte & 0x07),
            // BIT bit, r8
            0o100..=0o177 => BIT((byte >> 3) & 0x07, register(byte & 0x07)),
            // RES bit, r8
            0o200..=0o277 => RES((byte >> 3) & 0x07, register(byte & 0x07)),
            // SET bit, r8
            0o300..=0o377 => SET((byte >> 3) & 0x07, register(byte & 0x07)),
        }
    }
}

mod jump {

    #[derive(Clone, Copy)]
    pub(crate) enum JumpCondition {
        Always,
        NotZero,
        Zero,
        NotCarry,
        Carry,
    }

    impl std::fmt::Debug for JumpCondition {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use JumpCondition::*;

            match self {
                Always => f.write_str(""),
                NotZero => f.write_str("NZ"),
                Zero => f.write_str("Z"),
                NotCarry => f.write_str("NC"),
                Carry => f.write_str("C"),
            }
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) enum JumpLocation {
        HL,
        ImmediateWord,
    }

    impl std::fmt::Debug for JumpLocation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use JumpLocation::*;

            match *self {
                HL => f.write_str("HL"),
                ImmediateWord => f.write_str("u16"),
            }
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum AllRegisters {
    Group1(Group1RegisterPair),
    Register(Register),
}

impl std::fmt::Debug for AllRegisters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AllRegisters::*;

        match self {
            Group1(rp) => write!(f, "{:?}", rp),
            Register(r) => write!(f, "{:?}", r),
        }
    }
}

mod alu {
    use super::table::Register;

    #[derive(Clone, Copy)]
    pub(crate) enum Source {
        Register(Register),
        ImmediateByte,
    }

    impl std::fmt::Debug for Source {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Source::*;
            match self {
                Register(r) => write!(f, "{:?}", r),
                ImmediateByte => f.write_str("u8"),
            }
        }
    }
}

mod add {
    use super::table::{Group1RegisterPair, Register};

    #[derive(Clone, Copy)]
    pub(crate) enum Target {
        HL,
        A,
        SP,
    }

    impl std::fmt::Debug for Target {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Target::*;

            match self {
                HL => f.write_str("HL"),
                A => f.write_str("A"),
                SP => f.write_str("SP"),
            }
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) enum Source {
        Group1(Group1RegisterPair),
        Register(Register),
        ImmediateSignedByte,
        ImmediateByte,
    }

    impl std::fmt::Debug for Source {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Source::*;

            match self {
                Group1(rp) => write!(f, "{:?}", rp),
                Register(r) => write!(f, "{:?}", r),
                ImmediateSignedByte => f.write_str("i8"),
                ImmediateByte => f.write_str("u8"),
            }
        }
    }
}

mod load {
    use super::table::{Group1RegisterPair, Group2RegisterPair, Register};

    #[derive(Clone, Copy)]
    pub(crate) enum Target {
        IndirectImmediateWord, // (u16)
        Group1(Group1RegisterPair),
        IndirectGroup2(Group2RegisterPair),
        A,
        Register(Register),
        IoWithImmediateOffset, // 0xFF00 + offset
        SP,
        IoWithC, // 0xFF00 + C
    }

    impl std::fmt::Debug for Target {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Target::*;

            match self {
                IndirectImmediateWord => f.write_str("(u16)"),
                Group1(rp) => write!(f, "{:?}", rp),
                IndirectGroup2(rp) => write!(f, "({:?})", rp),
                A => f.write_str("A"),
                Register(r) => write!(f, "{:?}", r),
                IoWithImmediateOffset => f.write_str("(0xFF00 + u8)"),
                SP => f.write_str("SP"),
                IoWithC => f.write_str("(0xFF00 + C)"),
            }
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) enum Source {
        SP,
        ImmediateWord, // u16
        ImmediateByte, // u8
        A,
        IndirectGroup2(Group2RegisterPair),
        Register(Register),
        IoWithImmediateOffset, // 0xFF00 + offset
        HL,
        IoWithC,
        IndirectImmediateWord, // (u16)
    }

    impl std::fmt::Debug for Source {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Source::*;

            match self {
                SP => f.write_str("SP"),
                ImmediateWord => f.write_str("u16"),
                ImmediateByte => f.write_str("u8"),
                A => f.write_str("A"),
                IndirectGroup2(rp) => write!(f, "({:?})", rp),
                Register(r) => write!(f, "{:?}", r),
                IoWithImmediateOffset => f.write_str("(0xFF00 + u8)"),
                HL => f.write_str("HL"),
                IoWithC => f.write_str("(0xFF00 + C)"),
                IndirectImmediateWord => f.write_str("(u16)"),
            }
        }
    }
}

mod table {
    use super::add::{Source as AddSource, Target as AddTarget};
    use super::alu::Source as AluSource;
    use super::{Instruction, JumpCondition};
    use crate::cpu::{Register as CpuRegister, RegisterPair};

    #[derive(Clone, Copy)]
    pub(crate) enum Group1RegisterPair {
        BC,
        DE,
        HL,
        SP,
    }

    impl std::fmt::Debug for Group1RegisterPair {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Group1RegisterPair::*;

            match self {
                BC => f.write_str("BC"),
                DE => f.write_str("DE"),
                HL => f.write_str("HL"),
                SP => f.write_str("SP"),
            }
        }
    }

    impl Group1RegisterPair {
        pub fn as_register_pair(&self) -> RegisterPair {
            use Group1RegisterPair::*;

            match self {
                BC => RegisterPair::BC,
                DE => RegisterPair::DE,
                HL => RegisterPair::HL,
                SP => RegisterPair::SP,
            }
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) enum Group2RegisterPair {
        BC,
        DE,
        IncrementHL,
        DecrementHL,
    }

    impl std::fmt::Debug for Group2RegisterPair {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Group2RegisterPair::*;

            match self {
                BC => f.write_str("BC"),
                DE => f.write_str("DE"),
                IncrementHL => f.write_str("HL+"),
                DecrementHL => f.write_str("HL-"),
            }
        }
    }

    impl Group2RegisterPair {
        pub fn as_register_pair(&self) -> RegisterPair {
            use Group2RegisterPair::*;

            match self {
                BC => RegisterPair::BC,
                DE => RegisterPair::DE,
                IncrementHL => RegisterPair::HL,
                DecrementHL => RegisterPair::HL,
            }
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) enum Group3RegisterPair {
        BC,
        DE,
        HL,
        AF,
    }

    impl std::fmt::Debug for Group3RegisterPair {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Group3RegisterPair::*;

            match self {
                BC => f.write_str("BC"),
                DE => f.write_str("DE"),
                HL => f.write_str("HL"),
                AF => f.write_str("AF"),
            }
        }
    }

    impl Group3RegisterPair {
        pub fn as_register_pair(&self) -> RegisterPair {
            use Group3RegisterPair::*;

            match self {
                BC => RegisterPair::BC,
                DE => RegisterPair::DE,
                HL => RegisterPair::HL,
                AF => RegisterPair::AF,
            }
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) enum Register {
        B,
        C,
        D,
        E,
        H,
        L,
        IndirectHL,
        A,
    }

    impl std::fmt::Debug for Register {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use Register::*;

            match self {
                B => f.write_str("B"),
                C => f.write_str("C"),
                D => f.write_str("D"),
                E => f.write_str("E"),
                H => f.write_str("H"),
                L => f.write_str("L"),
                IndirectHL => f.write_str("(HL)"),
                A => f.write_str("A"),
            }
        }
    }

    impl Register {
        pub fn cpu_register(&self) -> CpuRegister {
            use Register::*;

            match self {
                B => CpuRegister::B,
                C => CpuRegister::C,
                D => CpuRegister::D,
                E => CpuRegister::E,
                H => CpuRegister::H,
                L => CpuRegister::L,
                A => CpuRegister::A,
                IndirectHL => panic!("Register::HL doesn't map onto CpuRegister"),
            }
        }
    }

    pub(crate) fn group1(code: u8) -> Group1RegisterPair {
        use Group1RegisterPair::*;

        match code {
            0b00 => BC,
            0b01 => DE,
            0b10 => HL,
            0b11 => SP,
            _ => unreachable!("{:#04X} is not a valid Group 1 Register Pair", code),
        }
    }

    pub(crate) fn group2(code: u8) -> Group2RegisterPair {
        use Group2RegisterPair::*;

        match code {
            0b00 => BC,
            0b01 => DE,
            0b10 => IncrementHL,
            0b11 => DecrementHL,
            _ => unreachable!("{:#04X} is not a valid Group 2 Register Pair", code),
        }
    }

    pub(crate) fn group3(code: u8) -> Group3RegisterPair {
        use Group3RegisterPair::*;

        match code {
            0b00 => BC,
            0b01 => DE,
            0b10 => HL,
            0b11 => AF,
            _ => unreachable!("{:#04X} is not a valid Group 3 Register Pair", code),
        }
    }

    pub(crate) fn register(code: u8) -> Register {
        use Register::*;

        match code {
            0b000 => B,
            0b001 => C,
            0b010 => D,
            0b011 => E,
            0b100 => H,
            0b101 => L,
            0b110 => IndirectHL,
            0b111 => A,
            _ => unreachable!("{:#04X} is not a valid Register"),
        }
    }

    pub(crate) fn jump_cond(code: u8) -> JumpCondition {
        use JumpCondition::*;

        match code {
            0b00 => NotZero,
            0b01 => Zero,
            0b10 => NotCarry,
            0b11 => Carry,
            _ => unreachable!("{:#04X} is not a valid JumpCondition", code),
        }
    }

    pub(crate) fn flag_instr(code: u8) -> Instruction {
        use Instruction::*;

        match code {
            0b000 => RLCA,
            0b001 => RRCA,
            0b010 => RLA,
            0b011 => RRA,
            0b100 => DAA,
            0b101 => CPL,
            0b110 => SCF,
            0b111 => CCF,
            _ => unreachable!("{:#04X} is not a valid flag opcode code", code),
        }
    }

    pub(crate) fn alu_reg_instr(alu_code: u8, reg_code: u8) -> Instruction {
        use Instruction::*;

        match alu_code {
            0b000 => ADD(AddTarget::A, AddSource::Register(register(reg_code))),
            0b001 => ADC(AluSource::Register(register(reg_code))),
            0b010 => SUB(AluSource::Register(register(reg_code))),
            0b011 => SBC(AluSource::Register(register(reg_code))),
            0b100 => AND(AluSource::Register(register(reg_code))),
            0b101 => XOR(AluSource::Register(register(reg_code))),
            0b110 => OR(AluSource::Register(register(reg_code))),
            0b111 => CP(AluSource::Register(register(reg_code))),
            _ => unreachable!("{:#04X} is not a valid alu reg instruction code", alu_code),
        }
    }

    pub(crate) fn alu_imm_instr(code: u8) -> Instruction {
        use Instruction::*;

        match code {
            0b000 => ADD(AddTarget::A, AddSource::ImmediateByte),
            0b001 => ADC(AluSource::ImmediateByte),
            0b010 => SUB(AluSource::ImmediateByte),
            0b011 => SBC(AluSource::ImmediateByte),
            0b100 => AND(AluSource::ImmediateByte),
            0b101 => XOR(AluSource::ImmediateByte),
            0b110 => OR(AluSource::ImmediateByte),
            0b111 => CP(AluSource::ImmediateByte),
            _ => unreachable!("{:#04X} is not a valid alu imm instruction code", code),
        }
    }

    pub(crate) fn prefix_alu(alu_code: u8, reg_code: u8) -> Instruction {
        use Instruction::*;

        match alu_code {
            0b000 => RLC(register(reg_code)),
            0b001 => RRC(register(reg_code)),
            0b010 => RL(register(reg_code)),
            0b011 => RR(register(reg_code)),
            0b100 => SLA(register(reg_code)),
            0b101 => SRA(register(reg_code)),
            0b110 => SWAP(register(reg_code)),
            0b111 => SRL(register(reg_code)),
            _ => unreachable!("{:#04X} is not a valid pfx alu instruction code", alu_code),
        }
    }
}

pub(crate) mod cycle {

    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
    #[repr(transparent)]
    pub struct Cycle(u32);

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
