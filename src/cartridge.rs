use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

#[derive(Debug, Clone, Default)]
pub struct Cartridge {
    memory: Vec<u8>,
    mbc: Box<dyn MBC>,
}

impl Cartridge {
    pub fn new<P: AsRef<Path> + ?Sized>(path: &P) -> io::Result<Self> {
        let mut memory = vec![];
        let mut rom = File::open(path)?;
        rom.read_to_end(&mut memory)?;

        Ok(Self {
            mbc: Self::detect_mbc(&memory),
            memory,
        })
    }

    fn detect_mbc(memory: &[u8]) -> Box<dyn MBC> {
        let ram_size = Self::find_ram_size(&memory);
        let bank_count = Self::find_bank_count(&memory);
        let mbc_kind = Self::find_mbc(&memory);
        let ram_byte_count = ram_size.to_byte_count();

        let mbc = match mbc_kind {
            MBCKind::None => todo!("Handle no MBC Situation"),
            MBCKind::MBC1 => MBC1 {
                kind: mbc_kind,
                ram_size,
                ram: vec![0; ram_byte_count as usize].into_boxed_slice(),
                bank_count,
                ..Default::default()
            },
            MBCKind::MBC5 => todo!("Implement MBC5"),
        };

        Box::new(mbc)
    }

    fn find_ram_size(memory: &[u8]) -> RAMSize {
        let id = memory[0x0149];
        id.into()
    }

    fn find_bank_count(memory: &[u8]) -> BankCount {
        let id = memory[0x0148];
        id.into()
    }

    fn find_mbc(memory: &[u8]) -> MBCKind {
        let id = memory[0x0147];

        // TODO: Refactor this to match the other enums in this module
        match id {
            0x00 => MBCKind::None,
            0x01 => MBCKind::MBC1,
            0x19 => MBCKind::MBC5,
            _ => panic!("Cartridge uses an unknown Memory Bank Controller"),
        }
    }
}

impl Cartridge {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match self.mbc.handle_read(addr) {
            MBCResult::Address(addr) => self.memory[addr as usize],
            MBCResult::RamValue(byte) => byte,
        }
    }
    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.mbc.handle_write(addr, byte);
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr + 1) as u16) << 8 | self.read_byte(addr) as u16
    }

    pub fn write_word(&mut self, addr: u16, word: u16) {
        self.write_byte(addr + 1, (word >> 8) as u8);
        self.write_byte(addr, (word & 0x00FF) as u8);
    }
}

#[derive(Debug, Clone, Default)]
struct MBC1 {
    pub kind: MBCKind,
    current_rom_bank: u8, // 5-bit Number
    current_ram_bank: u8, // 2-bit number
    mode: bool,
    ram_size: RAMSize,
    ram: Box<[u8]>,
    bank_count: BankCount,
    ram_enabled: bool,
}

impl MBC1 {
    fn calc_zero_bank_number(&self) -> u8 {
        match self.bank_count {
            BankCount::ThirtyTwo | BankCount::Sixteen | BankCount::Eight | BankCount::Four => 0,
            BankCount::SixtyFour => (self.current_ram_bank & 0x01) << 5,
            BankCount::OneHundredTwentyEight => self.current_ram_bank << 5,
            _ => unreachable!("{#:?} is not a valid ROM Bank Number for MBC1"),
        }
    }

    fn calc_high_bank_number(&self) -> u8 {
        match self.bank_count {
            BankCount::ThirtyTwo | BankCount::Sixteen | BankCount::Eight | BankCount::Four => {
                self.apply_rom_size_bitmask(self.current_rom_bank)
            }
            BankCount::SixtyFour => {
                let mut num = self.apply_rom_size_bitmask(self.current_rom_bank);
                num &= !(0x01 << 5);
                num | ((self.current_ram_bank & 0x01) << 5)
            }
            BankCount::OneHundredTwentyEight => {
                let mut num = self.apply_rom_size_bitmask(self.current_rom_bank);
                num &= !(0x03 << 5);
                num | ((self.current_ram_bank) << 5)
            }
            _ => unreachable!("{#:?} is not a valid ROM Bank Number for MBC1"),
        }
    }

    fn apply_rom_size_bitmask(&self, byte: u8) -> u8 {
        match self.bank_count {
            BankCount::Four => byte & 0b00000011,
            BankCount::Eight => byte & 0b00000111,
            BankCount::Sixteen => byte & 0b00001111,
            BankCount::ThirtyTwo => byte & 0b00011111,
            BankCount::SixtyFour => byte & 0b00011111,
            BankCount::OneHundredTwentyEight => byte & 0b00011111,
            _ => unreachable!("{#:?} is not a valid ROM Bank Number for MBC1"),
        }
    }
}

impl MBC for MBC1 {
    fn handle_read(&self, addr: u16) -> MBCResult {
        match addr {
            0x0000..=0x3FFF => {
                if self.mode {
                    let zero_bank_number = self.calc_zero_bank_number() as u16;
                    MBCResult::Address(0x4000 * zero_bank_number + addr)
                } else {
                    MBCResult::Address(addr)
                }
            }
            0x4000..=0x7FFF => {
                let high_bank_number = self.calc_high_bank_number() as u16;
                MBCResult::Address(0x4000 * high_bank_number * (addr - 0x4000))
            }
            0xA000..=0xBFFF => {
                if self.ram_enabled {
                    let ram_addr = match self.ram_size {
                        RAMSize::_2KB | RAMSize::_8KB => {
                            (addr as u32 - 0xA000) % self.ram_size.to_byte_count()
                        }
                        RAMSize::_32KB => {
                            if self.mode {
                                0x2000 * self.current_ram_bank as u32 + (addr as u32 - 0xA000)
                            } else {
                                addr as u32 - 0xA000
                            }
                        }
                        _ => unreachable!(""),
                    };

                    MBCResult::RamValue(self.ram[ram_addr as usize])
                } else {
                    MBCResult::Address(0x00FF)
                }
            }
            _ => unimplemented!(),
        }
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x1FFF => self.ram_enabled = (byte & 0x0F) == 0x0A,
            0x2000..=0x3FFF => {
                self.current_rom_bank = self.apply_rom_size_bitmask(byte);

                if self.current_rom_bank == 0 {
                    self.current_rom_bank = 1;
                }
            }
            0x4000..=0x5FFF => self.current_ram_bank = byte & 0x03,
            0x6000..=0x7FFF => self.mode = byte >> 7 == 0x01,
            0xA000..=0xBFFF => {
                if self.ram_enabled {
                    let ram_addr = match self.ram_size {
                        RAMSize::_2KB | RAMSize::_8KB => {
                            (addr as u32 - 0xA000) % self.ram_size.to_byte_count()
                        }
                        RAMSize::_32KB => {
                            if self.mode {
                                0x2000 * (self.current_ram_bank as u32) + (addr as u32 - 0xA000)
                            } else {
                                addr as u32 - 0xA000
                            }
                        }
                        _ => unreachable!("RAMSize can not be greater than 32KB on MBC1"),
                    };

                    self.ram[ram_addr as usize] = byte;
                }
            }
            _ => unreachable!("{:#06X} should not be handled by MBC1", addr),
        }
    }
}

trait MBC {
    fn handle_read(&self, addr: u16) -> MBCResult;
    fn handle_write(&mut self, addr: u16, byte: u8);
}

enum MBCResult {
    Address(u16),
    RamValue(u8),
}

#[derive(Debug, Clone, Copy)]
enum MBCKind {
    None,
    MBC1,
    MBC5,
}

impl Default for MBCKind {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, Copy)]
enum RAMSize {
    None = 0,
    _2KB = 1,
    _8KB = 2,
    _32KB = 3,  // Split into 4 RAM banks
    _128KB = 4, // Split into 16 RAM banks
    _64KB = 5,  // Split into 8 RAm Banks
}

impl RAMSize {
    pub fn to_byte_count(&self) -> u32 {
        match *self {
            RAMSize::None => 0,
            RAMSize::_2KB => 2_048,
            RAMSize::_8KB => 8_192,
            RAMSize::_32KB => 32_768,
            RAMSize::_128KB => 131_072,
            RAMSize::_64KB => 65_536,
        }
    }
}

impl Default for RAMSize {
    fn default() -> Self {
        Self::None
    }
}

impl From<u8> for RAMSize {
    fn from(byte: u8) -> Self {
        match byte {
            0x00 => Self::None,
            0x01 => Self::_2KB,
            0x02 => Self::_8KB,
            0x03 => Self::_32KB,
            0x04 => Self::_64KB,
            0x05 => Self::_128KB,
            _ => unreachable!("{:#04X} is an invalid value for RAMSize"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BankCount {
    None = 0,                  // 32KB
    Four = 1,                  // 64KB
    Eight = 2,                 // 128KB
    Sixteen = 3,               // 256KB
    ThirtyTwo = 4,             // 512KB
    SixtyFour = 5,             // 1MB
    OneHundredTwentyEight = 6, // 2MB
    TwoHundredFiftySix = 7,    // 4MB
    FiveHundredTwelve = 8,     // 8MB
    SeventyTwo = 0x52,         // 1.1MB
    Eighty = 0x53,             // 1.2MB
    NinetySix = 0x54,          // 1.5MB
}

impl Default for BankCount {
    fn default() -> Self {
        Self::None
    }
}

impl BankCount {
    // https://hacktix.github.io/GBEDG/mbcs/#rom-size
    pub fn to_byte_count(&self) -> u32 {
        match *self {
            BankCount::None => 32_768,
            BankCount::Four => 65_536,
            BankCount::Eight => 131_072,
            BankCount::Sixteen => 262_144,
            BankCount::ThirtyTwo => 524_288,
            BankCount::SixtyFour => 1_048_576,
            BankCount::OneHundredTwentyEight => 2_097_152,
            BankCount::TwoHundredFiftySix => 4_194_304,
            BankCount::FiveHundredTwelve => 8_388_608,
            BankCount::SeventyTwo => 1_179_648,
            BankCount::Eighty => 1_310_720,
            BankCount::NinetySix => 1_572_864,
        }
    }
}

impl From<u8> for BankCount {
    fn from(byte: u8) -> Self {
        match byte {
            0x00 => Self::None,
            0x01 => Self::Four,
            0x02 => Self::Eight,
            0x03 => Self::Sixteen,
            0x04 => Self::ThirtyTwo,
            0x05 => Self::SixtyFour,
            0x06 => Self::OneHundredTwentyEight,
            0x07 => Self::TwoHundredFiftySix,
            0x08 => Self::FiveHundredTwelve,
            0x52 => Self::SeventyTwo,
            0x53 => Self::Eighty,
            0x54 => Self::NinetySix,
            _ => unreachable!("{:#04X} is an invalid value for BankCount"),
        }
    }
}

impl std::fmt::Debug for Box<dyn MBC> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl std::clone::Clone for Box<dyn MBC> {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl std::default::Default for Box<dyn MBC> {
    fn default() -> Self {
        todo!()
    }
}
