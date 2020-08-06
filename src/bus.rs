#[derive(Debug, Copy, Clone)]
pub struct MemoryBus {}

impl MemoryBus {
    pub fn read_byte(&self, _address: u16) -> u8 {
        unimplemented!()
    }

    pub fn write_byte(&mut self, _address: u16) {
        unimplemented!()
    }

    pub fn read_word(&self, _address: u16) -> u16 {
        unimplemented!()
    }

    pub fn write_word(&mut self, __address: u16) {
        unimplemented!()
    }
}
