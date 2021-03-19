use super::cartridge::Cartridge;
use super::high_ram::HighRam;
use super::instruction::Cycles;
use super::interrupt::Interrupt;
use super::ppu::Ppu;
use super::serial::Serial;
use super::sound::Sound;
use super::timer::Timer;
use super::work_ram::{VariableWorkRam, WorkRam};
use std::{convert::TryInto, fs::File, io::Read};

#[derive(Debug, Clone)]
pub struct Bus {
    boot: Option<[u8; 256]>, // Boot ROM is 256b long
    cartridge: Option<Cartridge>,
    pub ppu: Ppu,
    wram: WorkRam,
    vwram: VariableWorkRam,
    timer: Timer,
    interrupt: Interrupt,
    sound: Sound,
    hram: HighRam,
    serial: Serial,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            boot: None,
            cartridge: None,
            ppu: Default::default(),
            wram: Default::default(),
            vwram: Default::default(),
            timer: Default::default(),
            interrupt: Default::default(),
            sound: Default::default(),
            hram: Default::default(),
            serial: Default::default(),
        }
    }
}

impl Bus {
    pub fn with_boot(path: &str) -> Self {
        let mut file = File::open(path).unwrap();
        let mut buf = Vec::with_capacity(256);
        file.read_to_end(&mut buf).unwrap();

        let boot_rom: [u8; 256] = buf.try_into().unwrap();

        Self {
            boot: Some(boot_rom),
            ..Default::default()
        }
    }

    pub fn load_cartridge(&mut self, path: &str) {
        self.cartridge = Some(Cartridge::new(path).unwrap());
    }

    pub fn step(&mut self, cycles: Cycles) {
        self.timer.step(cycles);
        self.sound.step(cycles);
        self.ppu.step(cycles);
    }
}

impl Bus {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => {
                // 16KB ROM bank 00
                if addr < 0x00FF {
                    if let Some(boot) = self.boot {
                        return boot[addr as usize];
                    }
                }

                match self.cartridge.as_ref() {
                    Some(cart) => cart.read_byte(addr),
                    None => panic!("Tried to read from a non-existant cartridge"),
                }
            }
            0x4000..=0x7FFF => match self.cartridge.as_ref() {
                // 16KB ROM Bank 01 -> NN (switchable via MB)
                Some(cart) => cart.read_byte(addr),
                None => panic!("Tried to read from a non-existant cartridge"),
            },
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                self.ppu.read_byte(addr)
            }
            0xA000..=0xBFFF => match self.cartridge.as_ref() {
                // 8KB External RAM
                Some(cart) => cart.read_byte(addr),
                None => panic!("Tried to read from the external RAM of a non-existant cartridge"),
            },
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                self.wram.read_byte(addr)
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                self.vwram.read_byte(addr)
            }
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF
                unimplemented!("Unable to read {:#06X} in Restricted Mirror", addr);
            }
            0xFE00..=0xFE9F => {
                // Sprite Attribute Table
                unimplemented!("Unable to read {:#06X} in the Sprite Attribute Table", addr);
            }
            0xFEA0..=0xFEFF => unimplemented!("{:#06X} is not allowed to be used", addr),
            0xFF00..=0xFF7F => {
                // IO Registers
                match addr {
                    0xFF01 => self.serial.next,
                    0xFF02 => self.serial.control.into(),
                    0xFF07 => self.timer.control.into(),
                    0xFF0F => self.interrupt.flag.into(),
                    0xFF11 => self.sound.ch1.sound_duty.into(),
                    0xFF12 => self.sound.ch1.vol_envelope.into(),
                    0xFF14 => self.sound.ch1.freq_hi.into(),
                    0xFF24 => self.sound.control.channel.into(),
                    0xFF25 => self.sound.control.output.into(),
                    0xFF26 => self.sound.control.status.into(),
                    0xFF40 => self.ppu.lcd_control.into(),
                    0xFF41 => self.ppu.stat.into(),
                    0xFF42 => self.ppu.pos.scroll_y,
                    0xFF43 => self.ppu.pos.scroll_x,
                    0xFF44 => self.ppu.pos.line_y,
                    0xFF45 => self.ppu.pos.ly_compare as u8,
                    0xFF47 => self.ppu.monochrome.bg_palette.into(),
                    0xFF48 => self.ppu.monochrome.obj_palette_0.into(),
                    0xFF49 => self.ppu.monochrome.obj_palette_1.into(),
                    0xFF4A => self.ppu.pos.window_y,
                    0xFF4B => self.ppu.pos.window_x,
                    _ => unimplemented!("Unable to read {:#06X} in I/O Registers", addr),
                }
            }
            0xFF80..=0xFFFE => {
                // High RAM
                self.hram.read_byte(addr)
            }
            0xFFFF => {
                // Interrupts Enable Register
                self.interrupt.enable.into()
            }
        }
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x3FFF => {
                // 16KB ROM bank 00
                match self.cartridge.as_mut() {
                    Some(cart) => cart.write_byte(addr, byte),
                    None => panic!("Tried to write into non-existent Cartridge"),
                }
            }
            0x4000..=0x7FFF => {
                // 16KB ROM Bank 01 -> NN (switchable via MB)
                match self.cartridge.as_mut() {
                    Some(cart) => cart.write_byte(addr, byte),
                    None => panic!("Tried to write into non-existent Cartridge"),
                }
            }
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                self.ppu.write_byte(addr, byte);
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                match self.cartridge.as_mut() {
                    Some(cart) => cart.write_byte(addr, byte),
                    None => panic!("Tried to write into non-existent Cartridge"),
                }
            }
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                self.wram.write_byte(addr, byte);
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                self.vwram.write_byte(addr, byte);
            }
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF
                unimplemented!("Unable to write to {:#06X} in Restricted Mirror", addr);
            }
            0xFE00..=0xFE9F => {
                // Sprite Attribute Table
                unimplemented!(
                    "Unable to write to {:#06X} in the Sprite Attribute Table",
                    addr
                );
            }
            0xFEA0..=0xFEFF => unimplemented!("{:#06X} is not allowed to be used", addr),
            0xFF00..=0xFF7F => {
                // IO Registers
                match addr {
                    0xFF01 => self.serial.next = byte,
                    0xFF02 => self.serial.control = byte.into(),
                    0xFF07 => self.timer.control = byte.into(),
                    0xFF0F => self.interrupt.flag = byte.into(),
                    0xFF11 => self.sound.ch1.sound_duty = byte.into(),
                    0xFF12 => self.sound.ch1.vol_envelope = byte.into(),
                    0xFF13 => self.sound.ch1.freq_lo = byte.into(),
                    0xFF14 => self.sound.ch1.freq_hi = byte.into(),
                    0xFF24 => self.sound.control.channel = byte.into(),
                    0xFF25 => self.sound.control.output = byte.into(),
                    0xFF26 => self.sound.control.status = byte.into(), // FIXME: Should we control which bytes are written to here?
                    0xFF40 => self.ppu.lcd_control = byte.into(),
                    0xFF41 => self.ppu.stat = byte.into(),
                    0xFF42 => self.ppu.pos.scroll_y = byte,
                    0xFF43 => self.ppu.pos.scroll_x = byte,
                    0xFF44 => self.ppu.pos.line_y = byte,
                    0xFF45 => self.ppu.pos.ly_compare = byte == 0x01, // FIXME: We don't consider the possibility of a byte being different form 0x00 or 0x01
                    0xFF47 => self.ppu.monochrome.bg_palette = byte.into(),
                    0xFF48 => self.ppu.monochrome.obj_palette_0 = byte.into(),
                    0xFF49 => self.ppu.monochrome.obj_palette_1 = byte.into(),
                    0xFF4A => self.ppu.pos.window_y = byte,
                    0xFF4B => self.ppu.pos.window_x = byte,
                    0xFF50 => {
                        // Disable Boot ROM
                        if byte != 0 {
                            self.boot = None;
                        }
                    }
                    _ => unimplemented!("Unable to write to {:#06X} in I/O Registers", addr),
                };
            }
            0xFF80..=0xFFFE => {
                // High RAM
                self.hram.write_byte(addr, byte);
            }
            0xFFFF => {
                // Interrupts Enable Register
                self.interrupt.enable = byte.into();
            }
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr + 1) as u16) << 8 | self.read_byte(addr) as u16
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        self.write_byte(addr + 1, (word >> 8) as u8);
        self.write_byte(addr, (word & 0x00FF) as u8);
    }
}
