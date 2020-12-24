use super::cartridge::Cartridge;

#[derive(Debug, Clone)]
pub struct Bus {
    boot: Option<[u8; 256]>, // Boot ROM is 256b long
    cartridge: Option<Cartridge>,
    vram: [u8; 8192], // 8KB of VRAM
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            boot: Some(include_bytes!("../bin/DMG_ROM.bin").to_owned()),
            cartridge: None,
            vram: [0; 8192],
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
                self.vram[(addr - 0x8000) as usize]
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                unimplemented!("Unable to read {:#06X} in Extermal RAM", addr);
            }
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                unimplemented!("Unable to read {:#06X} in Work RAM Bank 0", addr);
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                unimplemented!("Unable to read {:#06X} in Work RAM Bank N", addr);
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
                unimplemented!("Unable to read {:#06X} in I/O Registers", addr);
            }
            0xFF80..=0xFFFE => {
                // High RAM
                unimplemented!("Unable to read {:#06X} in High RAM", addr);
            }
            0xFFFF => {
                // Interupts Enable Register
                unimplemented!("Unable to read IE Register {:#06X} ", addr);
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
                self.vram[(addr - 0x8000) as usize] = byte;
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                unimplemented!("Unable to write to {:#06X} in Extermal RAM", addr);
            }
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                unimplemented!("Unable to write to {:#06X} in Work RAM Bank 0", addr);
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                unimplemented!("Unable to write to {:#06X} in Work RAM Bank N", addr);
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
                unimplemented!("Unable to write to {:#06X} in I/O Registers", addr);
            }
            0xFF80..=0xFFFE => {
                // High RAM
                unimplemented!("Unable to write to {:#06X} in High RAM", addr);
            }
            0xFFFF => {
                // Interupts Enable Register
                unimplemented!("Unable to write to IE Register {:#06X} ", addr);
            }
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        match addr {
            0x0000..=0x3FFF => {
                // 16KB ROM bank 00
                if addr <= 0x00FF && self.boot.is_some() {
                    let boot = self.boot.unwrap();
                    (boot[(addr + 1) as usize] as u16) << 8 | boot[addr as usize] as u16
                } else {
                    match &self.cartridge {
                        Some(cart) => cart.read_word(addr),
                        None => panic!("Tried to read from a non-existant cartridge"),
                    }
                }
            }
            0x4000..=0x7FFF => match &self.cartridge {
                // 16KB ROM Bank 01 -> NN (switchable via MB)
                Some(cart) => cart.read_word(addr),
                None => panic!("Tried to read from a non-existant cartridge"),
            },
            0x8000..=0x9FFF => {
                // 8KB Video RAM
                (self.vram[((addr + 1) - 0x8000) as usize] as u16) << 8
                    | self.vram[(addr - 0x8000) as usize] as u16
            }
            0xA000..=0xBFFF => {
                // 8KB External RAM
                unimplemented!("Unable to read {:#06X} in Extermal RAM", addr);
            }
            0xC000..=0xCFFF => {
                // 4KB Work RAM Bank 0
                unimplemented!("Unable to read {:#06X} in Work RAM Bank 0", addr);
            }
            0xD000..=0xDFFF => {
                // 4KB Work RAM Bank 1 -> N
                unimplemented!("Unable to read {:#06X} in Work RAM Bank N", addr);
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
                unimplemented!("Unable to read {:#06X} in I/O Registers", addr);
            }
            0xFF80..=0xFFFE => {
                // High RAM
                unimplemented!("Unable to read {:#06X} in High RAM", addr);
            }
            0xFFFF => {
                // Interupts Enable Register
                unimplemented!("Unable to read IE Register {:#06X} ", addr);
            }
        }
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        unimplemented!("Can't write {:#06X} to {:#06X}", word, addr)
    }
}
