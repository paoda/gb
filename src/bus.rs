#[derive(Debug, Copy, Clone)]
pub struct Bus {
    boot: [u8; 256],
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            boot: include_bytes!("../bin/DMG_ROM.bin").to_owned(),
        }
    }
}

impl Bus {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x00FF => {
                // Restart and Interrupt Vectors
                self.boot[addr as usize]
            }
            _ => unimplemented!("Can't read byte from {:#06x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr {
            0x000..=0x0FF => {
                // Restart and Itterupt Vectors
                self.boot[addr as usize] = byte;
            }
            _ => unimplemented!("Can't write {:#04x} to {:#06x}", byte, addr),
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        match addr {
            0x0000..=0x00FF => {
                // Restart and Interrupt Vectors
                (self.boot[(addr + 1) as usize] as u16) << 8 | self.boot[addr as usize] as u16
            }
            _ => unimplemented!("Can't read word from {:#06x}", addr),
        }
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        unimplemented!("Can't write {:#06x} to {:#06x}", word, addr)
    }
}
