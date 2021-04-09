const WORK_RAM_SIZE: usize = 0x1000;
const VARIABLE_WORK_RAM_SIZE: usize = WORK_RAM_SIZE;
const WORK_RAM_START_ADDRESS: usize = 0xC000;
const VARIABLE_WORK_RAM_START_ADDRESS: usize = 0xD000;

#[derive(Debug, Clone)]
pub struct WorkRam {
    bank: Box<[u8; WORK_RAM_SIZE]>,
}

impl WorkRam {
    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.bank[addr as usize - WORK_RAM_START_ADDRESS] = byte;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.bank[addr as usize - WORK_RAM_START_ADDRESS]
    }
}

impl Default for WorkRam {
    fn default() -> Self {
        Self {
            bank: Box::new([0u8; WORK_RAM_SIZE]),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BankNumber {
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
}

#[derive(Debug, Clone)]
pub struct VariableWorkRam {
    current: BankNumber,
    bank_n: Box<[[u8; VARIABLE_WORK_RAM_SIZE]; 7]>, // 4K for Variable amount of Banks (Banks 1 -> 7) in Game Boy Colour
}

impl Default for VariableWorkRam {
    fn default() -> Self {
        Self {
            current: BankNumber::One,
            bank_n: Box::new([[0u8; VARIABLE_WORK_RAM_SIZE]; 7]),
        }
    }
}

impl VariableWorkRam {
    pub fn set_current_bank(&mut self, bank: BankNumber) {
        self.current = bank;
    }

    pub fn get_current_bank(&self) -> BankNumber {
        self.current
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.bank_n[self.current as usize][addr as usize - VARIABLE_WORK_RAM_START_ADDRESS] = byte;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.bank_n[self.current as usize][addr as usize - VARIABLE_WORK_RAM_START_ADDRESS]
    }
}
