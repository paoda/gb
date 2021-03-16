#[derive(Debug, Clone)]
pub struct HighRam {
    buf: Box<[u8]>,
}

impl Default for HighRam {
    fn default() -> Self {
        Self {
            buf: vec![0u8; 128].into_boxed_slice(),
        }
    }
}

impl HighRam {
    pub fn write_byte(&mut self, index: usize, byte: u8) {
        self.buf[index] = byte;
    }

    pub fn read_byte(&self, index: usize) -> u8 {
        self.buf[index]
    }
}
