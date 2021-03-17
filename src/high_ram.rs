#[derive(Debug, Clone)]
pub struct HighRam {
    buf: Box<[u8; 127]>,
}

impl Default for HighRam {
    fn default() -> Self {
        Self {
            buf: Box::new([0u8; 127]),
        }
    }
}

impl HighRam {
    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.buf[addr as usize - 0xFF80] = byte;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.buf[addr as usize - 0xFF80]
    }
}
