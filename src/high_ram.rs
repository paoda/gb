#[derive(Debug, Clone)]
pub struct HighRAM {
    buf: Box<[u8]>,
}

impl Default for HighRAM {
    fn default() -> Self {
        Self {
            buf: vec![0u8; 127].into_boxed_slice(),
        }
    }
}

impl HighRAM {
    pub fn write_byte(&mut self, index: usize, byte: u8) {
        self.buf[index] = byte;
    }

    pub fn read_byte(&self, index: usize) -> u8 {
        self.buf[index]
    }
}
