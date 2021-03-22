use super::bus::Bus;
use super::instruction::{Cycles, Instruction, JumpCondition};
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

    pub fn boot_new(path: &str) -> Self {
        Self {
            bus: Bus::with_boot(path),
            ..Default::default()
        }
    }

    pub fn ime(&self) -> bool {
        self.ime
    }

    pub fn set_ime(&mut self, enabled: bool) {
        self.ime = enabled;
    }

    pub fn inc_pc(&mut self) {
        self.reg.pc += 1;
    }

    pub fn load_cartridge(&mut self, path: &str) {
        self.bus.load_cartridge(path);
    }
}

impl Cpu {
    pub fn fetch(&mut self) -> u8 {
        let opcode = self.bus.read_byte(self.reg.pc);
        self.inc_pc();

        opcode
    }

    pub fn decode(&mut self, opcode: u8) -> Instruction {
        Instruction::from_byte(self, opcode)
    }

    pub fn execute(&mut self, instruction: Instruction) -> Cycles {
        Instruction::execute(self, instruction)
    }

    pub fn step(&mut self) -> Cycles {
        let opcode = self.fetch();
        let instr = self.decode(opcode);
        let cycles = self.execute(instr);

        self.bus.step(cycles);
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
        if self.ime() {
            use JumpCondition::Always;

            let mut req: InterruptFlag = self.read_byte(0xFF0F).into();
            let mut enabled: InterruptEnable = self.read_byte(0xFFFF).into();

            let vector = if req.vblank() && enabled.vblank() {
                // Handle VBlank Interrupt
                req.set_vblank(false);
                enabled.set_vblank(false);

                // INT 40h
                Some(0x0040)
            } else if req.lcd_stat() && enabled.lcd_stat() {
                // Handle LCD STAT Interrupt
                req.set_lcd_stat(false);
                enabled.set_lcd_stat(false);

                // INT 48h
                Some(0x0048)
            } else if req.timer() && enabled.timer() {
                // Handle Timer Interrupt
                req.set_timer(false);
                enabled.set_timer(false);

                // INT 50h
                Some(0x0050)
            } else if req.serial() && enabled.serial() {
                // Handle Serial Interrupt
                req.set_serial(false);
                enabled.set_serial(false);

                // INT 58h
                Some(0x0058)
            } else if req.joypad() && enabled.joypad() {
                // Handle Joypad Interrupt
                req.set_joypad(false);
                enabled.set_joypad(false);

                // INT 60h
                Some(0x0060)
            } else {
                None
            };

            let _ = match vector {
                Some(register) => {
                    //  Write the Changes to 0xFF0F and 0xFFFF registers
                    self.write_byte(0xFF0F, req.into());
                    self.write_byte(0xFFFF, enabled.into());

                    // Disable all future interrupts
                    self.set_ime(false);
                    self.execute(Instruction::CALL(Always, register))
                }
                None => Cycles::new(0), // NO Interrupts were enabled and / or requested
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
        flags.0
    }
}

impl From<u8> for Flags {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}
