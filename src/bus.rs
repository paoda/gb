#[derive(Debug, Copy, Clone)]
pub struct Bus {}

impl Bus {
    pub fn read_byte(&self, addr: u16) -> u8 {
        unimplemented!()
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        unimplemented!()
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        unimplemented!()
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        unimplemented!()
    }
}
