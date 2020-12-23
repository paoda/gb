const ROM: &[u8; 256] = include_bytes!("../bin/DMG_ROM.bin");

#[derive(Debug, Copy, Clone, Default)]
pub struct Bus {}

impl Bus {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x00FF => {
                // Restart and Interrupt Vectors
                ROM[addr as usize]
            }
            _ => unimplemented!(),
        }
    }

    pub fn write_byte(&mut self, _addr: u16, _byte: u8) {
        unimplemented!()
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        match addr {
            0x0000..=0x00FF => {
                // Restart and Interrupt Vectors
                (ROM[(addr + 1) as usize] as u16) << 8 | ROM[addr as usize] as u16
            }
            _ => unimplemented!(),
        }
    }

    pub fn write_word(&mut self, _addr: u16, _word: u16) {
        unimplemented!()
    }
}
