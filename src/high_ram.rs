const HIGH_RAM_SIZE: usize = 127;
const HIGH_RAM_START_ADDRESS: usize = 0xFF80;

#[derive(Debug, Clone)]
pub struct HighRam {
    buf: Box<[u8; HIGH_RAM_SIZE]>,
}

impl Default for HighRam {
    fn default() -> Self {
        Self {
            buf: Box::new([0u8; HIGH_RAM_SIZE]),
        }
    }
}

impl HighRam {
    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.buf[addr as usize - HIGH_RAM_START_ADDRESS] = byte;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.buf[addr as usize - HIGH_RAM_START_ADDRESS]
    }
}
