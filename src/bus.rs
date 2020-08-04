#[derive(Debug, Copy, Clone)]
pub struct MemoryBus {

}

impl MemoryBus {
    pub fn read_byte(&self, _address: u16) {
        unimplemented!()
    }

    pub fn write_byte(&self, _address: u16) {
        unimplemented!()
    }
}