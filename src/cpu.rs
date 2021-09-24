use crate::bus::{Bus, BusIo, BOOT_SIZE};
use crate::instruction::Instruction;
use crate::interrupt::{InterruptEnable, InterruptFlag};
use crate::Cycle;
use bitfield::bitfield;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Default)]
pub struct Cpu {
    pub bus: Bus,
    reg: Registers,
    flags: Flags,
    ime: ImeState,
    state: State,
}

impl Cpu {
    #[allow(dead_code)]
    pub(crate) fn without_boot() -> Self {
        Self {
            reg: Registers {
                a: 0x01,
                b: 0x00,
                c: 0x13,
                d: 0x00,
                e: 0xD8,
                h: 0x01,
                l: 0x4D,
                sp: 0xFFFE,
                pc: 0x0100,
            },
            flags: 0xb0.into(),
            ..Default::default()
        }
    }

    pub(crate) fn with_boot(rom: [u8; BOOT_SIZE]) -> Self {
        Self {
            bus: Bus::with_boot(rom),
            ..Default::default()
        }
    }

    pub(crate) fn ime(&self) -> ImeState {
        self.ime
    }

    pub(crate) fn set_ime(&mut self, state: ImeState) {
        self.ime = state;
    }

    pub(crate) fn halt_cpu(&mut self, kind: HaltKind) {
        self.state = State::Halt(kind);
    }

    fn resume_execution(&mut self) {
        self.state = State::Execute;
    }

    pub(crate) fn is_halted(&self) -> bool {
        matches!(self.state, State::Halt(_))
    }

    pub(crate) fn halt_kind(&self) -> Option<HaltKind> {
        match self.state {
            State::Halt(kind) => Some(kind),
            _ => None,
        }
    }
}

impl Cpu {
    /// Fetch an [Instruction] from the memory bus
    /// (4 cycles)
    fn fetch(&mut self) -> u8 {
        let byte = self.read_byte(self.reg.pc);
        self.bus.clock();
        self.reg.pc += 1;
        byte
    }

    /// Decode a byte into an [SM83](Cpu) [Instruction]
    ///
    /// If opcode == 0xCB, then decoding costs 4 cycles.
    /// Otherwise, decoding is free
    pub(crate) fn decode(&mut self, opcode: u8) -> Instruction {
        if opcode == 0xCB {
            Instruction::decode(self.fetch(), true)
        } else {
            Instruction::decode(opcode, false)
        }
    }

    /// Execute an [Instruction].
    ///
    /// The amount of cycles necessary to execute an instruction range from
    /// 0 to 20 T-cycles
    fn execute(&mut self, instruction: Instruction) -> Cycle {
        Instruction::execute(self, instruction)
    }

    /// Perform the [`Cpu::fetch()`] [`Cpu::decode()`] [`Cpu::execute()`]
    /// routine.
    ///
    /// Handle HALT and interrupts.
    pub fn step(&mut self) -> Cycle {
        // Log instructions
        // if self.reg.pc > 0xFF {
        //     let out = std::io::stdout();
        //     let _ = self._print_logs(out.lock());
        // }

        if let Some(elapsed) = self.handle_interrupt() {
            return elapsed;
        }

        if let Some(kind) = self.halt_kind() {
            use HaltKind::*;

            self.bus.clock();

            let elapsed = match kind {
                ImeEnabled | NonePending => 4,
                SomePending => todo!("Implement HALT bug"),
            };

            return elapsed;
        }

        let opcode = self.fetch();
        let instr = self.decode(opcode);
        let elapsed = self.execute(instr);
        self.handle_ei();

        // For use in Blargg's Test ROMs
        if self.read_byte(0xFF02) == 0x81 {
            let c = self.read_byte(0xFF01) as char;
            self.write_byte(0xFF02, 0x00);
            eprint!("{}", c);
        }

        elapsed
    }
}

impl BusIo for Cpu {
    fn read_byte(&self, addr: u16) -> u8 {
        self.bus.read_byte(addr)
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.bus.write_byte(addr, byte);
    }
}

impl Cpu {
    #[inline]
    pub(crate) fn bus(&self) -> &Bus {
        &self.bus
    }

    #[inline]
    pub(crate) fn bus_mut(&mut self) -> &mut Bus {
        &mut self.bus
    }

    fn handle_ei(&mut self) {
        match self.ime {
            ImeState::EiExecuted => self.ime = ImeState::Pending,
            ImeState::Pending => self.ime = ImeState::Enabled,
            ImeState::Disabled | ImeState::Enabled => {}
        }
    }

    pub(crate) fn int_request(&self) -> u8 {
        self.read_byte(0xFF0F)
    }

    pub(crate) fn int_enable(&self) -> u8 {
        self.read_byte(0xFFFF)
    }

    fn handle_interrupt(&mut self) -> Option<Cycle> {
        let irq = self.int_request();
        let enable = self.int_enable();

        // TODO: Ensure that this behaviour is correct
        if self.is_halted() {
            // When we're here either a HALT with IME set or
            // a HALT with IME not set and No pending Interrupts was called

            if irq & enable != 0 {
                // The if self.ime() below correctly follows the "resuming from HALT" behaviour so
                // nothing actually needs to be added here. This is just documentation
                // since it's a bit weird why nothing is being done

                self.resume_execution();
            }
        }

        match self.ime() {
            ImeState::Enabled => {
                let mut irq: InterruptFlag = irq.into();
                let enable: InterruptEnable = enable.into();

                let rst_vector = if irq.vblank() && enable.vblank() {
                    // Handle VBlank Interrupt
                    irq.set_vblank(false);

                    // INT 40h
                    Some(0x40)
                } else if irq.lcd_stat() && enable.lcd_stat() {
                    // Handle LCD STAT Interrupt
                    irq.set_lcd_stat(false);

                    // INT 48h
                    Some(0x48)
                } else if irq.timer() && enable.timer() {
                    // Handle Timer Interrupt
                    irq.set_timer(false);

                    // INT 50h
                    Some(0x50)
                } else if irq.serial() && enable.serial() {
                    // Handle Serial Interrupt
                    irq.set_serial(false);

                    // INT 58h
                    Some(0x58)
                } else if irq.joypad() && enable.joypad() {
                    // Handle Joypad Interrupt
                    irq.set_joypad(false);

                    // INT 60h
                    Some(0x60)
                } else {
                    None
                };

                match rst_vector {
                    Some(vector) => {
                        //  Write the Changes to 0xFF0F and 0xFFFF registers
                        self.write_byte(0xFF0F, irq.into());

                        // Disable all future interrupts
                        self.set_ime(ImeState::Disabled);
                        Some(Instruction::reset(self, vector))
                    }
                    None => None,
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum State {
    Execute,
    Halt(HaltKind),
    // Stop,
}

impl Default for State {
    fn default() -> Self {
        Self::Execute
    }
}

impl Cpu {
    pub(crate) fn set_register(&mut self, register: Register, value: u8) {
        use Register::*;

        match register {
            A => self.reg.a = value,
            B => self.reg.b = value,
            C => self.reg.c = value,
            D => self.reg.d = value,
            E => self.reg.e = value,
            H => self.reg.h = value,
            L => self.reg.l = value,
        }
    }

    pub(crate) fn register(&self, register: Register) -> u8 {
        use Register::*;

        match register {
            A => self.reg.a,
            B => self.reg.b,
            C => self.reg.c,
            D => self.reg.d,
            E => self.reg.e,
            H => self.reg.h,
            L => self.reg.l,
        }
    }

    pub(crate) fn register_pair(&self, pair: RegisterPair) -> u16 {
        use RegisterPair::*;

        match pair {
            AF => (self.reg.a as u16) << 8 | u8::from(self.flags) as u16,
            BC => (self.reg.b as u16) << 8 | self.reg.c as u16,
            DE => (self.reg.d as u16) << 8 | self.reg.e as u16,
            HL => (self.reg.h as u16) << 8 | self.reg.l as u16,
            SP => self.reg.sp,
            PC => self.reg.pc,
        }
    }

    pub(crate) fn set_register_pair(&mut self, pair: RegisterPair, value: u16) {
        use RegisterPair::*;

        let high = (value >> 8) as u8;
        let low = value as u8;

        match pair {
            AF => {
                self.reg.a = high;
                self.flags = low.into();
            }
            BC => {
                self.reg.b = high;
                self.reg.c = low;
            }
            DE => {
                self.reg.d = high;
                self.reg.e = low;
            }
            HL => {
                self.reg.h = high;
                self.reg.l = low;
            }
            SP => self.reg.sp = value,
            PC => self.reg.pc = value,
        }
    }

    pub(crate) fn flags(&self) -> &Flags {
        &self.flags
    }

    pub(crate) fn update_flags(&mut self, z: bool, n: bool, h: bool, c: bool) {
        self.flags.set_z(z);
        self.flags.set_n(n);
        self.flags.set_h(h);
        self.flags.set_c(c);
    }

    pub(crate) fn set_flags(&mut self, flags: Flags) {
        self.flags = flags;
    }
}

impl Cpu {
    fn _print_debug(&self, mut w: impl std::io::Write) -> std::io::Result<()> {
        write!(w, "A: {:02X} ", self.reg.a)?;
        write!(w, "F: {:02X} ", u8::from(self.flags))?;
        write!(w, "B: {:02X} ", self.reg.b)?;
        write!(w, "C: {:02X} ", self.reg.c)?;
        write!(w, "D: {:02X} ", self.reg.d)?;
        write!(w, "E: {:02X} ", self.reg.e)?;
        write!(w, "H: {:02X} ", self.reg.h)?;
        write!(w, "L: {:02X} ", self.reg.l)?;
        write!(w, "SP: {:04X} ", self.reg.sp)?;
        write!(w, "PC: 00:{:04X} ", self.reg.pc)?;
        write!(w, "({:02X} ", self.read_byte(self.reg.pc))?;
        write!(w, "{:02X} ", self.read_byte(self.reg.pc + 1))?;
        write!(w, "{:02X} ", self.read_byte(self.reg.pc + 2))?;
        write!(w, "{:02X})", self.read_byte(self.reg.pc + 3))?;
        writeln!(w, "| {:?}", self._dbg_instr())?;
        w.flush()
    }

    fn _print_logs(&self, mut w: impl std::io::Write) -> std::io::Result<()> {
        write!(w, "A: {:02X} ", self.reg.a)?;
        write!(w, "F: {:02X} ", u8::from(self.flags))?;
        write!(w, "B: {:02X} ", self.reg.b)?;
        write!(w, "C: {:02X} ", self.reg.c)?;
        write!(w, "D: {:02X} ", self.reg.d)?;
        write!(w, "E: {:02X} ", self.reg.e)?;
        write!(w, "H: {:02X} ", self.reg.h)?;
        write!(w, "L: {:02X} ", self.reg.l)?;
        write!(w, "SP: {:04X} ", self.reg.sp)?;
        write!(w, "PC: 00:{:04X} ", self.reg.pc)?;
        write!(w, "({:02X} ", self.read_byte(self.reg.pc))?;
        write!(w, "{:02X} ", self.read_byte(self.reg.pc + 1))?;
        write!(w, "{:02X} ", self.read_byte(self.reg.pc + 2))?;
        writeln!(w, "{:02X})", self.read_byte(self.reg.pc + 3))?;
        w.flush()
    }

    fn _dbg_instr(&self) -> Instruction {
        let byte = self.read_byte(self.reg.pc);
        if byte == 0xCB {
            Instruction::decode(self.read_byte(self.reg.pc + 1), true)
        } else {
            Instruction::decode(byte, false)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum RegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Default)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
}

bitfield! {
    pub struct Flags(u8);
    impl Debug;
    pub z, set_z: 7; // Zero Flag
    pub n, set_n: 6; // Subtraction Flag
    pub h, set_h: 5; // Half Carry Flag
    pub c, set_c: 4; // Carry Flag
}

impl Flags {
    pub(crate) fn update(&mut self, z: bool, n: bool, h: bool, c: bool) {
        self.set_z(z);
        self.set_n(n);
        self.set_h(h);
        self.set_c(c);
    }
}

impl Copy for Flags {}
impl Clone for Flags {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for Flags {
    fn default() -> Self {
        Self(0)
    }
}

impl Display for Flags {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if self.z() {
            f.write_str("Z")?;
        } else {
            f.write_str("_")?;
        }

        if self.n() {
            f.write_str("N")?;
        } else {
            f.write_str("_")?;
        }

        if self.h() {
            f.write_str("H")?;
        } else {
            f.write_str("_")?;
        }

        if self.c() {
            f.write_str("C")
        } else {
            f.write_str("_")
        }
    }
}

impl From<Flags> for u8 {
    fn from(flags: Flags) -> Self {
        flags.0 & 0xF0
    }
}

impl From<u8> for Flags {
    fn from(byte: u8) -> Self {
        Self(byte & 0xF0)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum HaltKind {
    ImeEnabled,
    NonePending,
    SomePending,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ImeState {
    Disabled,
    EiExecuted,
    Pending,
    Enabled,
}

impl Default for ImeState {
    fn default() -> Self {
        Self::Disabled
    }
}
