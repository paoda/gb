#[derive(Debug, Clone)]
pub struct WorkRAM {
    bank: Box<[u8]>,
}

impl WorkRAM {
    pub fn write_byte(&mut self, index: usize, byte: u8) {
        self.bank[index] = byte;
    }

    pub fn read_byte(&self, index: usize) -> u8 {
        self.bank[index]
    }
}

impl Default for WorkRAM {
    fn default() -> Self {
        Self {
            bank: vec![0u8; 4096].into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BankNumber {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
}

impl From<BankNumber> for usize {
    fn from(bank_num: BankNumber) -> Self {
        match bank_num {
            BankNumber::One => 1,
            BankNumber::Two => 2,
            BankNumber::Three => 3,
            BankNumber::Four => 4,
            BankNumber::Five => 5,
            BankNumber::Six => 6,
            BankNumber::Seven => 7,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableWorkRAM {
    current: BankNumber,
    bank_n: Box<[[u8; 4096]]>, // 4K for Variable amount of Banks (Banks 1 -> 7) in Game Boy Colour
}

impl Default for VariableWorkRAM {
    fn default() -> Self {
        Self {
            current: BankNumber::One,
            bank_n: vec![[0u8; 4096]; 7].into_boxed_slice(),
        }
    }
}

impl VariableWorkRAM {
    pub fn set_current_bank(&mut self, bank: BankNumber) {
        self.current = bank;
    }

    pub fn get_current_bank(&self) -> BankNumber {
        self.current
    }

    pub fn write_byte(&mut self, index: usize, byte: u8) {
        let num: usize = self.current.into();
        self.bank_n[num][index] = byte;
    }

    pub fn read_byte(&self, index: usize) -> u8 {
        let num: usize = self.current.into();
        self.bank_n[num][index]
    }
}
