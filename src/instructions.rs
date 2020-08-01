use super::cpu::Flag;
/// Sharp SM83 Instructions
/// ### Definitions
/// * "Any valid 8-bit register" refers to the registers `B`, `C`, `D`, `E`, `H` and `L`.
///     * If `A` is also a valid register, it will be **explicitly** stated as such.
/// * "Any valid 16-bit register" refers to the registers BC, DE and HL.
///     * If `AF` is a valid register, it will be **explicitly** stated as such.
///     * If `SP` is a valid register, it will be **explicitly** stated as such.
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
    /// * `a` The A register
    /// * `n` Any valid 8-bit register (including `A`),`(BC)`, `(DE)`, `(HL)`, `(nn)`, and `#`.
    ///     * `nn` A two byte immediate value (Least significant byte first).
    pub fn ld_a_n(a: &mut u8, n: u8) {
        unimplemented!()
    }

    /// `LD n, A` Store register A into n.
    /// ### Arguments
    /// * `n` Any valid 8-bit register (including `A`), `(BC)`, `(DE)`, `(HL)`, and `(nn)`.
    ///     * `nn` A two byte immediate value (Least significant byte first).
    /// * `a` The A register.
    pub fn ld_n_a(n: &mut u8, a: u8) {
        unimplemented!()
    }

    /// `LD A, (C)` Store value at $FF00 + register C in register A.
    /// ### Arguments
    /// * `a` The A register.
    /// * `c` The C register.
    pub fn ld_a_c(a: &mut u8, c: u8) {
        unimplemented!()
    }

    /// `LD (C) ,A` Store the value of register A into $FF00 + register C.
    /// ### Arguments
    /// * `c` The C register.
    /// * `a` The A register.
    pub fn ld_c_a(c: u8, a: u8) {
        unimplemented!()
    }

    // `LD A, (HLD)` is identical to `LDD A, (HL)`.
    // `LD A, (HL-)` is identical to `LDD A, (HL)`.

    /// `LDD A, (HL)` Put value at $HL into A, then decrement HL.
    ///
    /// Identical to `LD A, (HLD)`, and `LD A, (HL-)`.
    /// ### Arguments
    /// * `a` The A register.
    /// * `hl` The HL register
    pub fn ldd_a_hl(a: &mut u8, hl: u16) {
        unimplemented!()
    }

    // `LD (HLD), A` is identical to `LDD (HL), A`.
    // `LD (HL-), A` is identical to `LDD (HL), A`.

    /// `LDD (HL), A` Store register A in $HL, then decrement HL.
    ///
    /// Identical to `LD (HLD), A`, and `LD (HL-), A`
    /// ### Arguments
    /// * `hl` The HL register.
    /// * `a` The A register.
    pub fn ldd_hl_a(hl: u16, a: u8) {
        unimplemented!()
    }

    // `LD A, (HLI)` is identical to `LDI A, (HL)`.
    // `LD A, (HL+)` is identical to `LDI A, (HL)`.

    /// `LDI A, (HL)` Store value at $HL in register A, then increment HL.
    ///
    /// Identical to `LD A, (HLI)`, and `LD A, (HL+)`.
    /// ### Arguments
    /// * `a` The A register.
    /// * `hl` The HL register.
    pub fn ldi_a_hl(a: &mut u8, hl: u16) {
        unimplemented!()
    }

    // `LD (HLI), A` is identical to `LDI (HL), A`.
    // `LD (HL+), A` is identical to `LDI (HL), A`.

    /// `LDI (HL), A` Store register A in $HL, then increment HL.
    ///
    /// Identical to `LD (HLI), A`, and `LD (HL+), A`.
    /// ### Arguments
    /// * `hl` The HL register.
    /// * `a` The A register.
    pub fn ldi_hl_a(hl: u16, a: u8) {
        unimplemented!()
    }

    /// `LDH (n), A` Store register A into address $FF00 + n.
    /// ### Arguments
    /// * `n` An 8-bit immediate value.
    /// * `a` The A register.
    pub fn ldh_n_a(n: u8, a: u8) {
        unimplemented!()
    }

    /// `LDH A, (n)` Store address $FF00 + n in the A register.
    /// ### Arguments
    /// * `a` The A register.
    /// * `n` An 8-bit immediate value
    pub fn ldh_a_n(a: &mut u8, n: u8) {
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
    /// * `sp` The stack pointer register.
    /// * `hl` The HL register.
    pub fn ld_sp_hl(sp: &mut u16, hl: u16) {
        unimplemented!()
    }

    // `LD HL, SP + n` is identical to `LDHL SP, n`.

    /// `LDHL SP, n` "Put SP + n effective address into HL".
    /// ### Arguments
    /// * `hl` The HL register.
    /// * `sp` The stack pointer register.
    /// * `n` 8-bit **signed** integer.
    /// * `f` CPU flags.
    ///
    /// ### Flags
    /// * `ZF` Reset.
    /// * `N` Reset.
    /// * `H` Set / Reset depending on operation.
    /// * `CY` Set / Reset depending on operation.
    pub fn ldhl_sp_n(hl: &mut u16, sp: u16, n: i8, f: &mut Flag) {
        unimplemented!()
    }

    /// `LD (nn), SP` Store stack pointer at $nn.
    /// ### Arguments
    /// * `nn` A 16-bit immediate address.
    /// * `sp` The stack pointer register.
    pub fn ld_nn_sp(nn: u16, sp: u16) {
        unimplemented!()
    }

    /// `PUSH nn` Push 16-bit register onto the stack, then
    /// decrement the stack pointer twice.
    /// ### Arguments
    /// * `nn` Any valid 16-bit address (including `AF`).
    /// * `sp` The stack pointer register.
    /// * `stack` The stack.
    pub fn push_nn(nn: u16, sp: &mut u16, stack: &[u16]) {
        unimplemented!()
    }

    /// `POP nn` Pop two bytes of the stack into register pair nn,
    /// then increment the stack pointer twice.
    /// ### Arguments
    /// `nn` Any valid 16-bit address (including `AF`).
    /// `sp` The stack pointer register.
    /// `stack` The stack.
    pub fn pop_nn(nn: &mut u16, sp: &mut u16, stack: &[u16]) {
        unimplemented!()
    }

    // *** 8-bit ALU ***

    /// `ADD A, n` Add n to register A.
    /// ### Arguments
    /// `A` The A register
    /// `nn` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero..
    /// `N` Reset.
    /// `H` Set if carry from bit 3.
    /// `CY` Set if carry from bit 7.
    pub fn add_a_n(a: &mut u8, n: u8) {
        unimplemented!()
    }

    /// `ADC A, n` Add n + carry flag to register A.
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero.
    /// `N` Reset.
    /// `H` Set if carry from bit 3.
    /// `CY` Set if carry from bit 7.
    pub fn adc_a_n(a: &mut u8, n: u8, f: &mut Flag) {
        unimplemented!()
    }

    /// `SUB n` or `SUB A, n` Subtract n from register A.
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is 0.
    /// `N` Set.
    /// `H` Set if no borrow from bit 4.
    /// `CY` Set if no borrow.
    pub fn sub_a_n(a: &mut u8, n: u8) {
        unimplemented!()
    }

    /// `SBC A, n` Subtract n + carry flag from register A.
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is 0.
    /// `N` Set.
    /// `H` Set if no borrow from bit 4.
    /// `CY` Set if no borrow.
    pub fn sbc_a_n(a: &mut u8, n: u8, f: &mut Flag) {
        unimplemented!()
    }

    /// `AND n` or `AND A, n` AND n with register A,
    /// then store the result in register A.
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero.
    /// `N` Reset
    /// `H` Set
    /// `CY` Reset
    pub fn and_a_n(a: &mut u8, n: u8, f: &mut Flag) {
        unimplemented!()
    }

    /// `OR n` or `OR A, n` OR n with register A, then store the result in register A
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero.
    /// `N` Reset.
    /// `H` Reset.
    /// `CY` Reset.
    pub fn or_a_n(a: &mut u8, n: u8, f: &mut Flag) {
        unimplemented!()
    }

    /// `XOR n` or `XOR A, n` XOR n with register A, then store the result in register A.
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid -bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero.
    /// `N` Reset.
    /// `H` Reset.
    /// `CY` Reset.
    pub fn xor_a_n(a: &mut u8, n: u8) {
        unimplemented!()
    }

    /// `CP n` or `CP A, n` Compare register A with n.
    ///
    /// Note: This is equivalent with `SUB A, n` except that the result of the
    /// subtraction is discarded. Only the flags are mutated.
    /// ### Arguments
    /// `A` The A register.
    /// `n` Any valid 8-bit register (including `A`), `(HL)`, and `#`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero ( A == n ).
    /// `N` Set.
    /// `H` Set if no borrow from bit 4.
    /// `CY` Set for no borrow. ( A < n ).
    pub fn cp_a_n(a: u8, n: u8) {
        unimplemented!()
    }

    /// `INC n` Increment register n.
    /// ### Arguments
    /// `n` Any valid 8-bit register (including `A`), and `(HL)`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero.
    /// `N` Reset.
    /// `H` Set if carry from bit 3.
    /// `CY` Not affected.
    pub fn inc_n(n: &mut u8) {
        unimplemented!()
    }

    /// `DEC n` decrement register n.
    /// ### Arguments
    /// `n` Any 8-bit register (including `A`), and `(HL)`.
    ///
    /// ### Flags
    /// `ZF` Set if result is zero.
    /// `N` Set.
    /// `H` Set if no borrow from bit 4.
    /// `CY` Not affected.
    pub fn dec_n(n: &mut u8) {
        unimplemented!()
    }

    // *** 16-bit Arithmetic ***

    /// `ADD HL, n` Add n to HL.
    /// ### Arguments
    /// `hl` The HL register.
    /// `n` Any valid 16-bit address (including `SP`).
    ///
    /// ### Flags
    /// `ZF` Not affected
    /// `N` Reset
    /// `H` Set if carry from bit 11.
    /// `CY` Set if carry from bit 15.
    pub fn add_hl_n(hl: &mut u16, n: u16) {
        unimplemented!()
    }

    /// `ADD SP, n` Add n to the stack pointer.
    /// ### Arguments
    /// `sp` The stack pointer register.
    /// `n` AN 8-bit signed immediate value (#).
    ///
    /// ### Flags
    /// `ZF` Reset
    /// `N` Reset
    /// `H` Set or reset according to operation.
    /// `CY` Set or reset according to operation.
    pub fn add_sp_n(sp: &mut u16, n: i8) {
        unimplemented!()
    }

    /// `INC nn` Increment register nn
    /// ### Arguments
    /// `nn` Any valid 16-bit register (including `SP`)
    pub fn inc_nn(nn: &mut u16) {
        unimplemented!()
    }

    /// `DEC nn` Increment register nn
    /// ### Arguments
    /// `nn` Any valid 16-bit register (including `SP`)
    pub fn dec_nn(nn: &mut u16) {
        unimplemented!()
    }

    // *** Miscellaneous ***
}
