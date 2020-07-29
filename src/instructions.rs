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
    /// * `n` Any valid 8-bit register (including `A`),`(BC)`, `(DE)`, `(HL)`, `(nn)`, and #.
    ///     * `nn` A two byte immediate value (Least significant byte first).
    pub fn ld_A_n(A: &mut u8, n: u8) {
        unimplemented!()
    }

    /// `LD n, A` Store register A into n.
    /// ### Arguments
    /// * `n` Any valid 8-bit register (including `A`), `(BC)`, `(DE)`, `(HL)`, and `(nn)`.
    ///     * `nn` A two byte immediate value (Least significant byte first).
    /// * `A` The A register.
    pub fn ld_n_A(n: &mut u8, A: u8) {
        unimplemented!()
    }

    /// `LD A, (C)` Store value at $FF00 + register C in register A.
    /// ### Arguments
    /// * `A` The A register.
    /// * `C` The C register.
    pub fn ld_A_C(A: &mut u8, C: u8) {
        unimplemented!()
    }

    /// `LD (C) ,A` Store the value of register A into $FF00 + register C.
    /// ### Arguments
    /// * `C` The C register.
    /// * `A` The A register.
    pub fn ld_C_A(C: u8, A: u8) {
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
    pub fn ldd_A_HL(A: &mut u8, HL: u16) {
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
    pub fn ldd_HL_A(HL: u16, A: u8) {
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
    pub fn ldi_A_HL(A: &mut u8, HL: u16) {
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
    pub fn ldi_HL_A(HL: u16, A: u8) {
        unimplemented!()
    }

    /// `LDH (n), A` Store register A into address $FF00 + n.
    /// ### Arguments
    /// * `n` An 8-bit immediate value.
    /// * `A` The A register.
    pub fn ldh_n_A(n: u8, A: u8) {
        unimplemented!()
    }

    /// `LDH A, (n)` Store address $FF00 + n in the A register.
    /// ### Arguments
    /// * `A` The A register.
    /// * `n` An 8-bit immediate value
    pub fn ldh_A_n(A: &mut u8, n: u8) {
        unimplemented!()
    }

    // *** 16-bit Loads ***
}
