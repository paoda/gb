use crate::bus::BusIo;

const WORK_RAM_SIZE: usize = 0x1000;
const VARIABLE_WORK_RAM_SIZE: usize = WORK_RAM_SIZE;
const WORK_RAM_START_ADDRESS: usize = 0xC000;
const VARIABLE_WORK_RAM_START_ADDRESS: usize = 0xD000;

#[derive(Debug)]
pub(crate) struct WorkRam {
    bank: Box<[u8; WORK_RAM_SIZE]>,
}

impl BusIo for WorkRam {
    fn read_byte(&self, addr: u16) -> u8 {
        self.bank[addr as usize - WORK_RAM_START_ADDRESS]
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.bank[addr as usize - WORK_RAM_START_ADDRESS] = byte;
    }
}

impl Default for WorkRam {
    fn default() -> Self {
        Self {
            bank: Box::new([0u8; WORK_RAM_SIZE]),
        }
    }
}

#[derive(Debug)]
pub(crate) struct VariableWorkRam {
    buf: Box<[u8; VARIABLE_WORK_RAM_SIZE]>, // 4K for Variable amount of Banks (Banks 1 -> 7) in Game Boy Colour
}

impl Default for VariableWorkRam {
    fn default() -> Self {
        Self {
            buf: Box::new([0u8; VARIABLE_WORK_RAM_SIZE]),
        }
    }
}

impl BusIo for VariableWorkRam {
    fn read_byte(&self, addr: u16) -> u8 {
        self.buf[addr as usize - VARIABLE_WORK_RAM_START_ADDRESS]
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.buf[addr as usize - VARIABLE_WORK_RAM_START_ADDRESS] = byte;
    }
}
