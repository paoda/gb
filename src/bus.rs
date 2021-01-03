use super::cartridge::Cartridge;
use super::interrupt::Interrupt;
use super::ppu::PPU;
use super::sound::Sound;
use super::timer::Timer;
use super::work_ram::{VariableWorkRAM, WorkRAM};

#[derive(Debug, Clone)]
pub struct Bus {
    boot: Option<[u8; 256]>, // Boot ROM is 256b long
    cartridge: Option<Cartridge>,
    ppu: PPU,
    wram: WorkRAM,
    vwram: VariableWorkRAM,
    timer: Timer,
    interrupt: Interrupt,
    sound: Sound,
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            boot: Some(include_bytes!("../bin/DMG_ROM.bin").to_owned()),
            cartridge: None,
            ppu: Default::default(),
            wram: Default::default(),
            vwram: Default::default(),
            timer: Default::default(),
            interrupt: Default::default(),
            sound: Default::default(),
        }
    }
}

impl Bus {
    pub fn with_boot() -> Self {
        Default::default()
    }

    pub fn without_boot() -> Self {
        Self {
            boot: None,
            ..Default::default()
        }
    }

    pub fn load_cartridge(&mut self, path: &str) {
        self.cartridge = Some(Cartridge::new(path).unwrap());
    }
}

impl Bus {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => {
                // 16KB ROM bank 00
                if addr <= 0x00FF && self.boot.is_some() {
                    let boot = self.boot.unwrap();
                    boot[addr as usize]
                } else {
                    match &self.cartridge {
                        Some(cart) => cart.read_byte(addr),
                        None => panic!("Tried to read from a non-existant cartridge"),
                    }
                }
            }
            0x4000..=0x7FFF => match &self.cartridge {
                // 16KB ROM Bank 01 -> NN (switchable via MB)
                Some(cart) => cart.read_byte(addr),
                None => panic!("Tried to read from a non-existant cartridge"),
            },
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                self.ppu.vram[(addr - 0x8000) as usize]
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                unimplemented!("Unable to read {:#06X} in Extermal RAM", addr);
            }
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                self.wram.read_byte((addr - 0xC000) as usize)
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                self.vwram.read_byte((addr - 0xD000) as usize)
            }
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF
                unimplemented!("Unable to read {:#06X} in Restricted Mirror", addr);
            }
            0xFE00..=0xFE9F => {
                // Sprite Attrbute Table
                unimplemented!("Unable to read {:#06X} in the Sprite Attribute Table", addr);
            }
            0xFEA0..=0xFEFF => unimplemented!("{:#06X} is not allowed to be used", addr),
            0xFF00..=0xFF7F => {
                // IO Registers
                match addr {
                    0xFF07 => self.timer.control.into(),
                    0xFF0F => self.interrupt.flag.into(),
                    0xFF26 => self.sound.status.into(),
                    _ => unimplemented!("Unable to read {:#06X} in I/O Registers", addr),
                }
            }
            0xFF80..=0xFFFE => {
                // High RAM
                unimplemented!("Unable to read {:#06X} in High RAM", addr);
            }
            0xFFFF => {
                // Interupts Enable Register
                self.interrupt.enable.into()
            }
        }
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x3FFF => {
                // 16KB ROM bank 00
                panic!("Tried to write to {:#06X} in ROM Bank 00", addr);
            }
            0x4000..=0x7FFF => {
                // 16KB ROM Bank 01 -> NN (switchable via MB)
                panic!("Tried to write to {:#06X} in ROM Bank 01 -> NN", addr);
            }
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                self.ppu.vram[(addr - 0x8000) as usize] = byte;
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                unimplemented!("Unable to write to {:#06X} in Extermal RAM", addr);
            }
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                self.wram.write_byte((addr - 0xC000) as usize, byte);
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                self.vwram.write_byte((addr - 0xD000) as usize, byte);
            }
            0xE000..=0xFDFF => {
                // Mirror of 0xC000 to 0xDDFF
                unimplemented!("Unable to write to {:#06X} in Restricted Mirror", addr);
            }
            0xFE00..=0xFE9F => {
                // Sprite Attrbute Table
                unimplemented!(
                    "Unable to write to {:#06X} in the Sprite Attribute Table",
                    addr
                );
            }
            0xFEA0..=0xFEFF => unimplemented!("{:#06X} is not allowed to be used", addr),
            0xFF00..=0xFF7F => {
                // IO Registers
                match addr {
                    0xFF07 => self.timer.control = byte.into(),
                    0xFF0F => self.interrupt.flag = byte.into(),
                    0xFF26 => self.sound.status = byte.into(), // FIXME: Should we control which bytes are written to here?
                    _ => unimplemented!("Unable to write to {:#06X} in I/O Registers", addr),
                };
            }
            0xFF80..=0xFFFE => {
                // High RAM
                unimplemented!("Unable to write to {:#06X} in High RAM", addr);
            }
            0xFFFF => {
                // Interupts Enable Register
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
