use super::bus::Bus;
use super::instruction::{Cycle, Instruction};
use super::interrupt::{InterruptEnable, InterruptFlag};
use super::ppu::Ppu;
use bitfield::bitfield;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone, Default)]
pub struct Cpu {
    pub bus: Bus,
    reg: Registers,
    flags: Flags,
    ime: bool,
    halted: Option<HaltState>,
    state: State,
}

impl Cpu {
    pub fn new() -> Self {
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

    pub fn boot_new(path: &str) -> anyhow::Result<Self> {
        Ok(Self {
            bus: Bus::with_boot(path)?,
            ..Default::default()
        })
    }

    pub fn ime(&self) -> bool {
        self.ime
    }

    pub fn set_ime(&mut self, enabled: bool) {
        self.ime = enabled;
    }

    pub fn halt(&mut self, state: HaltState) {
        self.halted = Some(state);
    }

    fn resume(&mut self) {
        println!("Game Boy resumed from {:?}", self.halted);

        self.halted = None;
    }

    pub fn halted(&self) -> Option<HaltState> {
        self.halted
    }

    pub fn inc_pc(&mut self) {
        self.reg.pc += 1;
    }

    pub fn load_cartridge(&mut self, path: &str) -> std::io::Result<()> {
        self.bus.load_cartridge(path)
    }
}

impl Cpu {
    pub fn fetch(&self) -> u8 {
        self.bus.read_byte(self.reg.pc)
    }

    pub fn decode(&mut self, opcode: u8) -> Instruction {
        Instruction::from_byte(self, opcode)
    }

    pub fn execute(&mut self, instruction: Instruction) -> Cycle {
        Instruction::execute(self, instruction)
    }

    pub fn step(&mut self) -> Cycle {
        if !self.bus.boot_enabled() {
            self.log_state().unwrap();
        }

        let cycles = match self.halted() {
            Some(state) => {
                use HaltState::*;

                match state {
                    ImeSet | NonePending => Cycle::new(4),
                    SomePending => todo!("Implement HALT bug"),
                }
            }
            None => {
                let opcode = self.fetch();
                self.inc_pc();

                let instr = self.decode(opcode);
                let cycles = self.execute(instr);

                self.bus.step(cycles);
                cycles
            }
        };

        self.handle_interrupts();

        cycles
    }
}

impl Cpu {
    pub fn read_imm_byte(&mut self, addr: u16) -> u8 {
        self.inc_pc();
        self.bus.read_byte(addr)
    }

    pub fn read_imm_word(&mut self, addr: u16) -> u16 {
        self.inc_pc();
        self.inc_pc();
        self.bus.read_word(addr)
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.bus.read_byte(addr)
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.bus.write_byte(addr, byte);
    }

    pub fn read_word(&mut self, addr: u16) -> u16 {
        self.bus.read_word(addr)
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        self.bus.write_word(addr, word)
    }
}

impl Cpu {
    pub fn get_ppu(&mut self) -> &mut Ppu {
        &mut self.bus.ppu
    }

    pub fn handle_interrupts(&mut self) {
        let req = self.read_byte(0xFF0F);
        let enabled = self.read_byte(0xFFFF);

        if self.halted.is_some() {
            // When we're here either a HALT with IME set or
            // a HALT with IME not set and No pending Interrupts was called

            if req & enabled != 0 {
                // The if self.ime() below correctly follows the "resuming from HALT" behaviour so
                // nothing actually needs to be added here. This is just documentation
                // since it's a bit weird why nothing is being done

                self.resume()
            }
        }

        if self.ime() {
            let mut req: InterruptFlag = req.into();
            let enabled: InterruptEnable = enabled.into();

            let vector = if req.vblank() && enabled.vblank() {
                // Handle VBlank Interrupt
                req.set_vblank(false);

                // INT 40h
                Some(0x40)
            } else if req.lcd_stat() && enabled.lcd_stat() {
                // Handle LCD STAT Interrupt
                req.set_lcd_stat(false);

                // INT 48h
                Some(0x48)
            } else if req.timer() && enabled.timer() {
                // Handle Timer Interrupt
                req.set_timer(false);

                // INT 50h
                Some(0x50)
            } else if req.serial() && enabled.serial() {
                // Handle Serial Interrupt
                req.set_serial(false);

                // INT 58h
                Some(0x58)
            } else if req.joypad() && enabled.joypad() {
                // Handle Joypad Interrupt
                req.set_joypad(false);

                // INT 60h
                Some(0x60)
            } else {
                None
            };

            let _ = match vector {
                Some(register) => {
                    //  Write the Changes to 0xFF0F and 0xFFFF registers
                    self.write_byte(0xFF0F, req.into());

                    // Disable all future interrupts
                    self.set_ime(false);
                    self.execute(Instruction::RST(register))
                }
                None => Cycle::new(0), // NO Interrupts were enabled and / or requested
            };
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum State {
    Execute,
    // Halt,
    // Stop,
}

impl Default for State {
    fn default() -> Self {
        Self::Execute
    }
}

impl Cpu {
    pub fn set_register(&mut self, register: Register, value: u8) {
        use Register::*;

        match register {
            A => self.reg.a = value,
            B => self.reg.b = value,
            C => self.reg.c = value,
            D => self.reg.d = value,
            E => self.reg.e = value,
            H => self.reg.h = value,
            L => self.reg.l = value,
            Flag => self.flags = value.into(),
        }
    }

    pub fn register(&self, register: Register) -> u8 {
        use Register::*;

        match register {
            A => self.reg.a,
            B => self.reg.b,
            C => self.reg.c,
            D => self.reg.d,
            E => self.reg.e,
            H => self.reg.h,
            L => self.reg.l,
            Flag => self.flags.into(),
        }
    }

    pub fn register_pair(&self, pair: RegisterPair) -> u16 {
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

    pub fn set_register_pair(&mut self, pair: RegisterPair, value: u16) {
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

    pub fn flags(&self) -> &Flags {
        &self.flags
    }

    pub fn set_flags(&mut self, flags: Flags) {
        self.flags = flags;
    }
}

impl Cpu {
    fn log_state(&self) -> std::io::Result<()> {
        use std::io::Write;

        let out = std::io::stdout();
        let mut handle = out.lock();

        write!(handle, "A: {:02X} ", self.reg.a)?;
        write!(handle, "F: {:02X} ", u8::from(self.flags))?;
        write!(handle, "B: {:02X} ", self.reg.b)?;
        write!(handle, "C: {:02X} ", self.reg.c)?;
        write!(handle, "D: {:02X} ", self.reg.d)?;
        write!(handle, "E: {:02X} ", self.reg.e)?;
        write!(handle, "H: {:02X} ", self.reg.h)?;
        write!(handle, "L: {:02X} ", self.reg.l)?;
        write!(handle, "SP: {:04X} ", self.reg.sp)?;
        write!(handle, "PC: 00:{:04X} ", self.reg.pc)?;
        write!(handle, "({:02X} ", self.read_byte(self.reg.pc))?;
        write!(handle, "{:02X} ", self.read_byte(self.reg.pc + 1))?;
        write!(handle, "{:02X} ", self.read_byte(self.reg.pc + 2))?;
        writeln!(handle, "{:02X})", self.read_byte(self.reg.pc + 3))?;
        handle.flush()?;

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Flag,
}

#[derive(Debug, Copy, Clone)]
pub enum RegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Copy, Clone, Default)]
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
    pub fn update(&mut self, z: bool, n: bool, h: bool, c: bool) {
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
pub enum HaltState {
    ImeSet,
    NonePending,
    SomePending,
}
