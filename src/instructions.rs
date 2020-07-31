use super::cpu::Flag;
/// Sharp SM83 Instructions
/// ### Definitions
/// * "Any valid 8-bit register" refers to the registers `B`, `C`, `D`, `E`, `H` and `L`.
///     * If `A` is also a valid register, it will be **explicity** stated as such.
/// * "Any valid 16-bit register" refers to the registers BC, DE and HL.
///     * If `AF` is a valid register, it will be **explicitly** stated as such.
///     * If `SP` is a valid register, it will be **explicity** stated as such.
/// * the value of any 16-bit register in brackets is the value of the data located at
///     the address of the 16-bit register.
///     * e.g. the value of `(HL)` would be what is at `memory[cpu.reg.get_hl()]`
///     * Since the value is from memory, the value is an 8-bit integer.
pub struct Instruction {}

impl Instruction {
    // *** 8-bit Loads ***

    /// `LD nn, n` Store value nn in n.
    /// ### Arguments
    /// * `nn` Any valid 8-bit or 16-bit register (including `SP`).
    /// * `n` An 8-bit immediate value.
    pub fn ld_nn_n(nn: u16, n: &mut u8) {
        unimplemented!()
    }

    /// `LD r, r` Store value r2 into r1.
    /// ### Arguments
    /// * `r1` Any valid 8-bit register (including `A`), and `(HL)`.
    /// * `r2` Any valid 8-bit register (including `A`), and `(HL)`.
    pub fn ld_r1_r2(r1: &mut u8, r2: u8) {
        unimplemented!()
    }

    /// `LD A, n` Store value n into register A.
    /// ### Arguments
    /// * `A` The A register
    /// * `n` Any valid 8-bit register (including `A`),`(BC)`, `(DE)`, `(HL)`, `(nn)`, and `#`.
    ///     * `nn` A two byte immediate value (Least significant byte first).
    pub fn ld_a_n(A: &mut u8, n: u8) {
        unimplemented!()
    }

    /// `LD n, A` Store register A into n.
    /// ### Arguments
    /// * `n` Any valid 8-bit register (including `A`), `(BC)`, `(DE)`, `(HL)`, and `(nn)`.
    ///     * `nn` A two byte immediate value (Least significant byte first).
    /// * `A` The A register.
    pub fn ld_n_a(n: &mut u8, A: u8) {
        unimplemented!()
    }

    /// `LD A, (C)` Store value at $FF00 + register C in register A.
    /// ### Arguments
    /// * `A` The A register.
    /// * `C` The C register.
    pub fn ld_a_c(A: &mut u8, C: u8) {
        unimplemented!()
    }

    /// `LD (C) ,A` Store the value of register A into $FF00 + register C.
    /// ### Arguments
    /// * `C` The C register.
    /// * `A` The A register.
    pub fn ld_c_a(C: u8, A: u8) {
        unimplemented!()
    }

    // `LD A, (HLD)` is identical to `LDD A, (HL)`.
    // `LD A, (HL-)` is identical to `LDD A, (HL)`.

    /// `LDD A, (HL)` Put value at $HL into A, then decrement HL.
    ///
    /// Identical to `LD A, (HLD)`, and `LD A, (HL-)`.
    /// ### Arguments
    /// * `A The A register.
    /// * `HL` The HL register
    pub fn ldd_a_hl(A: &mut u8, HL: u16) {
        unimplemented!()
    }

    // `LD (HLD), A` is identical to `LDD (HL), A`.
    // `LD (HL-), A` is identical to `LDD (HL), A`.

    /// `LDD (HL), A` Store register A in $HL, then decrement HL.
    ///
    /// Identical to `LD (HLD), A`, and `LD (HL-), A`
    /// ### Arguments
    /// * `HL` The HL register.
    /// * `A` The A register.
    pub fn ldd_hl_a(HL: u16, A: u8) {
        unimplemented!()
    }

    // `LD A, (HLI)` is identical to `LDI A, (HL)`.
    // `LD A, (HL+)` is identical to `LDI A, (HL)`.

    /// `LDI A, (HL)` Store value at $HL in register A, then increment HL.
    ///
    /// Identical to `LD A, (HLI)`, and `LD A, (HL+)`.
    /// ### Arguments
    /// * `A` The A register.
    /// * `HL` The HL register.
    pub fn ldi_a_hl(A: &mut u8, HL: u16) {
        unimplemented!()
    }

    // `LD (HLI), A` is identical to `LDI (HL), A`.
    // `LD (HL+), A` is identical to `LDI (HL), A`.

    /// `LDI (HL), A` Store register A in $HL, then increment HL.
    ///
    /// Identical to `LD (HLI), A`, and `LD (HL+), A`.
    /// ### Arguments
    /// * `HL` The HL register.
    /// * `A` The A register.
    pub fn ldi_hl_a(HL: u16, A: u8) {
        unimplemented!()
    }

    /// `LDH (n), A` Store register A into address $FF00 + n.
    /// ### Arguments
    /// * `n` An 8-bit immediate value.
    /// * `A` The A register.
    pub fn ldh_n_a(n: u8, A: u8) {
        unimplemented!()
    }

    /// `LDH A, (n)` Store address $FF00 + n in the A register.
    /// ### Arguments
    /// * `A` The A register.
    /// * `n` An 8-bit immediate value
    pub fn ldh_a_n(A: &mut u8, n: u8) {
        unimplemented!()
    }

    // *** 16-bit Loads ***

    /// `LD n, nn` Store value nn in n.(really?)
    /// ### Arguments
    /// * `n` Any 16-bit register (including `SP`).
    /// * `nn` A 16-bit immediate value.
    pub fn ld_n_nn(n: &mut u16, nn: u16) {
        unimplemented!()
    }

    /// `LD SP, HL` Put HL into the stack pointer.
    /// ### Arguments
    /// * `SP` The stack pointer register.
    /// * `HL` The HL register.
    pub fn ld_sp_hl(SP: &mut u16, HL: u16) {
        unimplemented!()
    }

    // `LD HL, SP + n` is identical to `LDHL SP, n`.

    /// `LDHL SP, n` "Put SP + n effective address into HL".
    /// ### Arguments
    /// * `HL` The HL register.
    /// * `SP` The stack pointer register.
    /// * `n` 8-bit **signed** integer.
    /// * `f` CPU flags.
    ///
    /// ### Flags
    /// * `ZF` Reset.
    /// * `N` Reset.
    /// * `H` Set / Reset depending on operation.
    /// * `CY` Set / Reset depending on operation.
    pub fn ldhl_sp_n(HL: &mut u16, SP: u16, n: i8, f: &mut Flag) {
        unimplemented!()
    }

    /// `LD (nn), SP` Store stack pointer at $nn.
    /// ### Arguments
    /// * `nn` A 16-bit immediate address.
    /// * `SP` The stack pointer register.
    pub fn ld_nn_sp(nn: u16, SP: u16) {
        unimplemented!()
    }

    /// `PUSH nn` Push 16-bit register onto the stack, then
    /// decrement the stack pointer twice.
    /// ### Arguments
    /// * `nn` Any valid 16-bit address (including AF).
    /// * `SP` The stack pointer register.
    /// * `stack` The stack.
    pub fn push_nn(nn: u16, SP: &mut u16, stack: &[u16]) {
        unimplemented!()
    }
}
