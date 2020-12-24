use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

#[derive(Debug, Clone)]
pub struct Cartridge {
    memory: Vec<u8>,
}

impl Cartridge {
    pub fn new<P: AsRef<Path> + ?Sized>(path: &P) -> io::Result<Self> {
        let mut memory = vec![];
        let mut rom = File::open(path)?;
        rom.read_to_end(&mut memory)?;

        Ok(Self { memory })
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.memory[(addr + 1) as usize] as u16) << 8 | self.memory[addr as usize] as u16
    }
}
