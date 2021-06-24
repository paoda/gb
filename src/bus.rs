use crate::cartridge::Cartridge;
use crate::high_ram::HighRam;
use crate::interrupt::{Interrupt, InterruptFlag};
use crate::joypad::Joypad;
use crate::ppu::{Ppu, PpuMode};
use crate::serial::Serial;
use crate::sound::Sound;
use crate::timer::Timer;
use crate::work_ram::{VariableWorkRam, WorkRam};
use std::{fs::File, io::Read};

const BOOT_ROM_SIZE: usize = 0x100;

#[derive(Debug, Clone)]
pub struct Bus {
    boot: Option<[u8; BOOT_ROM_SIZE]>, // Boot ROM is 256b long
    cartridge: Option<Cartridge>,
    pub ppu: Ppu,
    work_ram: WorkRam,
    var_ram: VariableWorkRam,
    timer: Timer,
    int: Interrupt,
    snd: Sound,
    high_ram: HighRam,
    serial: Serial,
    pub joypad: Joypad,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            boot: None,
            cartridge: None,
            ppu: Default::default(),
            work_ram: Default::default(),
            var_ram: Default::default(),
            timer: Default::default(),
            int: Default::default(),
            snd: Default::default(),
            high_ram: Default::default(),
            serial: Default::default(),
            joypad: Default::default(),
        }
    }
}

impl Bus {
    pub(crate) fn with_boot(path: &str) -> anyhow::Result<Self> {
        let mut file = File::open(path)?;
        let mut boot_rom = [0u8; 256];

        file.read_exact(&mut boot_rom)?;

        Ok(Self {
            boot: Some(boot_rom),
            ..Default::default()
        })
    }

    pub(crate) fn load_cartridge(&mut self, path: &str) -> std::io::Result<()> {
        self.cartridge = Some(Cartridge::new(path)?);
        Ok(())
    }

    pub(crate) fn rom_title(&self) -> Option<&str> {
        self.cartridge.as_ref()?.title()
    }

    pub(crate) fn clock(&mut self) {
        self.ppu.clock();
        self.timer.clock();
        self.snd.clock(self.timer.divider);
        self.clock_dma();
    }

    fn clock_dma(&mut self) {
        if let Some((src_addr, dest_addr)) = self.ppu.dma.clock() {
            let byte = self.oam_read_byte(src_addr);
            self.oam_write_byte(dest_addr, byte);
        }
    }

    pub(crate) fn timer(&self) -> Timer {
        self.timer
    }
}

impl Bus {
    pub fn oam_read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => {
                // 16KB ROM bank 00 (ends at 0x3FFF)
                // and 16KB ROM Bank 01 -> NN (switchable via MB)
                if addr < 0x100 {
                    if let Some(boot) = self.boot {
                        return boot[addr as usize];
                    }
                }

                match self.cartridge.as_ref() {
                    Some(cart) => cart.read_byte(addr),
                    None => panic!("Tried to read from a non-existent cartridge"),
                }
            }
            0x8000..=0x9FFF => self.ppu.read_byte(addr), // 8KB Video RAM
            0xA000..=0xBFFF => match self.cartridge.as_ref() {
                // 8KB External RAM
                Some(cart) => cart.read_byte(addr),
                None => panic!("Tried to read from a non-existent cartridge"),
            },
            0xC000..=0xCFFF => self.work_ram.read_byte(addr), // 4KB Work RAM Bank 0
            0xD000..=0xDFFF => self.var_ram.read_byte(addr),  // 4KB Work RAM Bank 1 -> N
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF (ECHO RAM)
                let masked_addr = addr & 0x1FFF;
                let equiv_addr = 0xC000 + masked_addr;

                match masked_addr {
                    // 0xE000 ..= 0xEFFF
                    0x0000..=0x0FFF => {
                        // 4KB Work RAM Bank 0
                        self.work_ram.read_byte(equiv_addr)
                    }
                    // 0xF000 ..= 0xFDFF
                    0x1000..=0x1DFF => {
                        // 4KB Work RAM Bank 1 -> N
                        self.var_ram.read_byte(equiv_addr)
                    }
                    _ => unreachable!("{:#06X} was incorrectly handled by ECHO RAM", addr),
                }
            }
            _ => panic!("OAM Transfer abnormally tried reading from {:#06X}", addr),
        }
    }

    pub fn oam_write_byte(&mut self, addr: u16, byte: u8) {
        self.ppu.oam.write_byte(addr, byte);
    }
}

impl BusIo for Bus {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => {
                // 16KB ROM bank 00 (ends at 0x3FFF)
                // and 16KB ROM Bank 01 -> NN (switchable via MB)
                if addr < 0x100 {
                    if let Some(boot) = self.boot {
                        return boot[addr as usize];
                    }
                }

                match self.cartridge.as_ref() {
                    Some(cart) => cart.read_byte(addr),
                    None => panic!("Tried to read from a non-existent cartridge"),
                }
            }
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                match self.ppu.stat.mode() {
                    PpuMode::Drawing => 0xFF,
                    _ => self.ppu.read_byte(addr),
                }
            }
            0xA000..=0xBFFF => match self.cartridge.as_ref() {
                // 8KB External RAM
                Some(cart) => cart.read_byte(addr),
                None => panic!("Tried to read from a non-existent cartridge"),
            },
            0xC000..=0xCFFF => self.work_ram.read_byte(addr), // 4KB Work RAM Bank 0
            0xD000..=0xDFFF => self.var_ram.read_byte(addr),  // 4KB Work RAM Bank 1 -> N
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF (ECHO RAM)
                let masked_addr = addr & 0x1FFF;
                let equiv_addr = 0xC000 + masked_addr;

                match masked_addr {
                    // 0xE000 ..= 0xEFFF
                    0x0000..=0x0FFF => {
                        // 4KB Work RAM Bank 0
                        self.work_ram.read_byte(equiv_addr)
                    }
                    // 0xF000 ..= 0xFDFF
                    0x1000..=0x1DFF => {
                        // 4KB Work RAM Bank 1 -> N
                        self.var_ram.read_byte(equiv_addr)
                    }
                    _ => unreachable!("{:#06X} was incorrectly handled by ECHO RAM", addr),
                }
            }
            0xFE00..=0xFE9F => {
                // Sprite Attribute Table
                use PpuMode::{HBlank, VBlank};

                match self.ppu.stat.mode() {
                    HBlank | VBlank => self.ppu.oam.read_byte(addr),
                    _ => 0xFF,
                }
            }
            0xFEA0..=0xFEFF => {
                // Prohibited Memory
                use PpuMode::{HBlank, VBlank};

                match self.ppu.stat.mode() {
                    HBlank | VBlank => 0x00,
                    _ => 0xFF, // TODO: OAM Sprite bug now occurs on the DMG
                }
            }
            0xFF00..=0xFF7F => {
                // IO Registers

                // Every address here starts with 0xFF so we can just check the
                // low byte to figure out which register it is
                match addr & 0x00FF {
                    0x00 => self.joypad.p1,
                    0x01 => self.serial.next,
                    0x02 => self.serial.ctrl.into(),
                    0x04 => (self.timer.divider >> 8) as u8,
                    0x05 => self.timer.counter,
                    0x06 => self.timer.modulo,
                    0x07 => self.timer.ctrl.into(),
                    0x0F => self.interrupt_flag().into(),
                    0x10 => self.snd.ch1.sweep.into(),
                    0x11 => self.snd.ch1.duty(),
                    0x12 => self.snd.ch1.envelope.into(),
                    0x14 => self.snd.ch1.freq_hi(),
                    0x16 => self.snd.ch2.duty(),
                    0x17 => self.snd.ch2.envelope.into(),
                    0x19 => self.snd.ch2.freq_hi(),
                    0x1A => self.snd.ch3.enabled(),
                    0x1B => self.snd.ch3.len(),
                    0x1C => self.snd.ch3.volume(),
                    0x1E => self.snd.ch3.freq_hi(),
                    0x20 => self.snd.ch4.len(),
                    0x21 => self.snd.ch4.envelope.into(),
                    0x22 => self.snd.ch4.poly.into(),
                    0x23 => self.snd.ch4.freq_data(),
                    0x24 => self.snd.ctrl.channel.into(),
                    0x25 => self.snd.ctrl.output.into(),
                    0x26 => self.snd.ctrl.status.into(),
                    0x30..=0x3F => self.snd.ch3.ram[addr as usize - 0xFF30],
                    0x40 => self.ppu.ctrl.into(),
                    0x41 => self.ppu.stat.into(),
                    0x42 => self.ppu.pos.scroll_y,
                    0x43 => self.ppu.pos.scroll_x,
                    0x44 => self.ppu.pos.line_y,
                    0x45 => self.ppu.pos.ly_compare as u8,
                    0x46 => self.ppu.dma.start.into(),
                    0x47 => self.ppu.monochrome.bg_palette.into(),
                    0x48 => self.ppu.monochrome.obj_palette_0.into(),
                    0x49 => self.ppu.monochrome.obj_palette_1.into(),
                    0x4A => self.ppu.pos.window_y,
                    0x4B => self.ppu.pos.window_x,
                    _ => unimplemented!("Unable to read {:#06X} in I/O Registers", addr),
                }
            }
            0xFF80..=0xFFFE => {
                // High RAM
                self.high_ram.read_byte(addr)
            }
            0xFFFF => {
                // Interrupts Enable Register
                self.int.enable.into()
            }
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x7FFF => {
                // 16KB ROM bank 00 (ends at 0x3FFF)
                // and 16KB ROM Bank 01 -> NN (switchable via MB)
                match self.cartridge.as_mut() {
                    Some(cart) => cart.write_byte(addr, byte),
                    None => panic!("Tried to write into non-existent cartridge"),
                }
            }
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                match self.ppu.stat.mode() {
                    PpuMode::Drawing => {}
                    _ => self.ppu.write_byte(addr, byte),
                }
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                match self.cartridge.as_mut() {
                    Some(cart) => cart.write_byte(addr, byte),
                    None => panic!("Tried to write into non-existent cartridge"),
                }
            }
            0xC000..=0xCFFF => self.work_ram.write_byte(addr, byte), // 4KB Work RAM Bank 0
            0xD000..=0xDFFF => self.var_ram.write_byte(addr, byte),  // 4KB Work RAM Bank 1 -> N
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF (ECHO RAM)
                let masked_addr = addr & 0x1FFF;
                let equiv_addr = 0xC000 + masked_addr;

                match masked_addr {
                    // 0xE000 ..= 0xEFFF
                    0x0000..=0x0FFF => {
                        // 4KB Work RAM Bank 0
                        self.work_ram.write_byte(equiv_addr, byte);
                    }
                    // 0xF000 ..= 0xFDFF
                    0x1000..=0x1DFF => {
                        // 4KB Work RAM Bank 1 -> N
                        self.var_ram.write_byte(equiv_addr, byte);
                    }
                    _ => unreachable!("{:#06X} was incorrectly handled by ECHO RAM", addr),
                }
            }
            0xFE00..=0xFE9F => {
                // Sprite Attribute Table
                use PpuMode::{HBlank, VBlank};

                match self.ppu.stat.mode() {
                    HBlank | VBlank => self.ppu.oam.write_byte(addr, byte),
                    _ => {}
                }
            }
            0xFEA0..=0xFEFF => {} // TODO: As far as I know, writes to here do nothing.
            0xFF00..=0xFF7F => {
                // IO Registers

                // Every address here starts with 0xFF so we can just check the
                // low byte to figure out which register it is
                match addr & 0x00FF {
                    0x00 => self.joypad.update(byte),
                    0x01 => self.serial.next = byte,
                    0x02 => self.serial.ctrl = byte.into(),
                    0x04 => self.timer.divider = 0x0000,
                    0x05 => self.timer.counter = byte,
                    0x06 => self.timer.modulo = byte,
                    0x07 => self.timer.ctrl = byte.into(),
                    0x0F => self.set_interrupt_flag(byte),
                    0x10 => self.snd.ch1.sweep = byte.into(),
                    0x11 => self.snd.ch1.set_duty(byte),
                    0x12 => self.snd.ch1.envelope = byte.into(),
                    0x13 => self.snd.ch1.freq_lo = byte,
                    0x14 => self.snd.ch1.set_freq_hi(byte),
                    0x16 => self.snd.ch2.set_duty(byte),
                    0x17 => self.snd.ch2.envelope = byte.into(),
                    0x18 => self.snd.ch2.freq_lo = byte,
                    0x19 => self.snd.ch2.set_freq_hi(byte),
                    0x1A => self.snd.ch3.set_enabled(byte),
                    0x1B => self.snd.ch3.set_len(byte),
                    0x1C => self.snd.ch3.set_volume(byte),
                    0x1D => self.snd.ch3.freq_lo = byte,
                    0x1E => self.snd.ch3.set_freq_hi(byte),
                    0x20 => self.snd.ch4.set_len(byte),
                    0x21 => self.snd.ch4.envelope = byte.into(),
                    0x22 => self.snd.ch4.poly = byte.into(),
                    0x23 => self.snd.ch4.set_freq_data(byte),
                    0x24 => self.snd.ctrl.channel = byte.into(),
                    0x25 => self.snd.ctrl.output = byte.into(),
                    0x26 => self.snd.ctrl.status = byte.into(), // FIXME: Should we control which bytes are written to here?
                    0x30..=0x3F => self.snd.ch3.ram[addr as usize - 0xFF30] = byte,
                    0x40 => self.ppu.ctrl = byte.into(),
                    0x41 => self.ppu.stat.update(byte),
                    0x42 => self.ppu.pos.scroll_y = byte,
                    0x43 => self.ppu.pos.scroll_x = byte,
                    0x44 => self.ppu.pos.line_y = byte,
                    0x45 => {
                        // Update LYC
                        self.ppu.pos.ly_compare = byte;

                        // Update Coincidence Flag
                        let are_equal = self.ppu.pos.line_y == byte;
                        self.ppu.stat.set_coincidence(are_equal);

                        // If enabled, request a LCD STAT interrupt
                        if self.ppu.stat.coincidence_int() && are_equal {
                            self.ppu.int.set_lcd_stat(true);
                        }
                    }
                    0x46 => self.ppu.dma.start.update(byte, &mut self.ppu.dma.state),
                    0x47 => self.ppu.monochrome.bg_palette = byte.into(),
                    0x48 => self.ppu.monochrome.obj_palette_0 = byte.into(),
                    0x49 => self.ppu.monochrome.obj_palette_1 = byte.into(),
                    0x4A => self.ppu.pos.window_y = byte,
                    0x4B => self.ppu.pos.window_x = byte,
                    0x4D => {} // Writing to this address is useful on the CGB only
                    0x50 => {
                        // Disable Boot ROM
                        if byte != 0 {
                            self.boot = None;
                        }
                    }
                    0x7F => {} // Tetris tries to write to this non-existent IO Address
                    _ => unimplemented!("Unable to write to {:#06X} in I/O Registers", addr),
                };
            }
            0xFF80..=0xFFFE => {
                // High RAM
                self.high_ram.write_byte(addr, byte);
            }
            0xFFFF => {
                // Interrupts Enable Register
                self.int.enable = byte.into();
            }
        }
    }
}

impl Bus {
    pub(crate) fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr + 1) as u16) << 8 | self.read_byte(addr) as u16
    }

    pub(crate) fn write_word(&mut self, addr: u16, word: u16) {
        self.write_byte(addr + 1, (word >> 8) as u8);
        self.write_byte(addr, (word & 0x00FF) as u8);
    }
}

impl Bus {
    fn interrupt_flag(&self) -> InterruptFlag {
        // Read the current interrupt information from the PPU
        let vblank = self.ppu.int.vblank();
        let lcd_stat = self.ppu.int.lcd_stat();

        // Read the current interrupt information from the Joypad
        let joypad = self.joypad.interrupt();

        // Read the current interrupt information from the Timer
        let timer = self.timer.interrupt();

        // Copy the Interrupt Flag register 0xFF0F
        let mut flag = self.int.flag;

        // Update the flag to have the most accurate information
        flag.set_vblank(vblank);
        flag.set_lcd_stat(lcd_stat);
        flag.set_joypad(joypad);
        flag.set_timer(timer);
        flag
    }

    fn set_interrupt_flag(&mut self, byte: u8) {
        // Update the Interrupt register 0xFF0F
        self.int.flag = byte.into();

        let vblank = self.int.flag.vblank();
        let lcd_stat = self.int.flag.lcd_stat();
        let joypad = self.int.flag.joypad();
        let timer = self.int.flag.timer();

        // Update the PPU's instance of the following interrupts
        self.ppu.int.set_vblank(vblank);
        self.ppu.int.set_lcd_stat(lcd_stat);

        // Update the Joypad's instance of the following interrupts
        self.joypad.set_interrupt(joypad);

        // Update the Timer's instance of the following interrupts
        self.timer.set_interrupt(timer);
    }
}

pub(crate) trait BusIo {
    fn read_byte(&self, addr: u16) -> u8;
    fn write_byte(&mut self, addr: u16, byte: u8);
}
