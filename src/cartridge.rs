use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use crate::bus::BusIo;

const RAM_SIZE_ADDRESS: usize = 0x0149;
const ROM_SIZE_ADDRESS: usize = 0x0148;
const MBC_TYPE_ADDRESS: usize = 0x0147;
const ROM_TITLE_RANGE: std::ops::RangeInclusive<usize> = 0x0134..=0x0143;

#[derive(Debug, Default)]
pub(crate) struct Cartridge {
    memory: Vec<u8>,
    title: Option<String>,
    mbc: Box<dyn MBCIo>,
}

impl Cartridge {
    pub(crate) fn new<P: AsRef<Path> + ?Sized>(path: &P) -> io::Result<Self> {
        let mut memory = vec![];
        let mut rom = File::open(path)?;
        rom.read_to_end(&mut memory)?;

        let title = Self::find_title(&memory);
        eprintln!("Cartridge Title: {:?}", title);

        Ok(Self {
            mbc: Self::detect_mbc(&memory),
            title,
            memory,
        })
    }

    fn detect_mbc(memory: &[u8]) -> Box<dyn MBCIo> {
        let ram_size = Self::find_ram_size(memory);
        let bank_count = Self::find_bank_count(memory);
        let mbc_kind = Self::find_mbc(memory);
        let ram_byte_count = ram_size.len();

        eprintln!("Cartridge Ram Size: {} bytes", ram_size.len());
        eprintln!("Cartridge ROM Size: {} bytes", bank_count.size());
        eprintln!("MBC Type: {:?}", mbc_kind);

        match mbc_kind {
            MBCKind::None => Box::new(NoMBC {}),
            MBCKind::MBC1 => {
                let mbc = MBC1 {
                    ram_size,
                    ram: vec![0; ram_byte_count as usize],
                    bank_count,
                    ..Default::default()
                };

                Box::new(mbc)
            }
            MBCKind::MBC1WithBattery => {
                // TODO: Implement Saving
                let mbc = MBC1 {
                    ram_size,
                    ram: vec![0; ram_byte_count as usize],
                    bank_count,
                    ..Default::default()
                };

                Box::new(mbc)
            }
            MBCKind::MBC3WithBattery => {
                // TODO: Implement Saving
                let mbc = MBC3 {
                    ram_size,
                    ram: vec![0; ram_byte_count as usize],
                    ..Default::default()
                };

                Box::new(mbc)
            }
            MBCKind::MBC3 => {
                let mbc = MBC3 {
                    ram_size,
                    ram: vec![0; ram_byte_count as usize],
                    ..Default::default()
                };

                Box::new(mbc)
            }
            MBCKind::MBC5 => todo!("Implement MBC5"),
        }
    }

    fn find_title(memory: &[u8]) -> Option<String> {
        let slice = &memory[ROM_TITLE_RANGE];
        let with_nulls = std::str::from_utf8(slice).ok();
        let trimmed = with_nulls.map(|s| s.trim_matches('\0'));

        match trimmed {
            Some("") | None => None,
            Some(_) => trimmed.map(String::from),
        }
    }

    pub(crate) fn title(&self) -> Option<&str> {
        self.title.as_deref()
    }

    fn find_ram_size(memory: &[u8]) -> RamSize {
        let id = memory[RAM_SIZE_ADDRESS];
        id.into()
    }

    fn find_bank_count(memory: &[u8]) -> BankCount {
        let id = memory[ROM_SIZE_ADDRESS];
        id.into()
    }

    fn find_mbc(memory: &[u8]) -> MBCKind {
        let id = memory[MBC_TYPE_ADDRESS];

        // TODO: Refactor this to match the other enums in this module
        match id {
            0x00 => MBCKind::None,
            0x01 => MBCKind::MBC1,
            0x02 => MBCKind::MBC1,
            0x03 => MBCKind::MBC1WithBattery,
            0x19 => MBCKind::MBC5,
            0x13 => MBCKind::MBC3WithBattery,
            0x11 => MBCKind::MBC3,
            _ => unimplemented!("id {:#04X} is an unsupported MBC", id),
        }
    }
}

impl BusIo for Cartridge {
    fn read_byte(&self, addr: u16) -> u8 {
        use MBCResult::*;

        match self.mbc.handle_read(addr) {
            Address(addr) => self.memory[addr],
            Value(byte) => byte,
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.mbc.handle_write(addr, byte);
    }
}

#[derive(Debug)]
struct MBC1 {
    /// 5-bit number
    rom_bank: u8,
    /// 2-bit number
    ram_bank: u8,
    mode: bool,
    ram_size: RamSize,
    ram: Vec<u8>,
    bank_count: BankCount,
    ram_enabled: bool,
}

impl Default for MBC1 {
    fn default() -> Self {
        Self {
            rom_bank: 0x01,
            ram_bank: Default::default(),
            mode: Default::default(),
            ram_size: Default::default(),
            ram: Default::default(),
            bank_count: Default::default(),
            ram_enabled: Default::default(),
        }
    }
}

impl MBC1 {
    fn zero_bank(&self) -> u8 {
        use BankCount::*;

        match self.bank_count {
            None | Four | Eight | Sixteen | ThirtyTwo => 0x00,
            SixtyFour => (self.ram_bank & 0x01) << 5,
            OneHundredTwentyEight => (self.ram_bank & 0x03) << 5,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.bank_count),
        }
    }

    fn _mbcm_zero_bank(&self) -> u8 {
        use BankCount::*;

        match self.bank_count {
            None | Four | Eight | Sixteen | ThirtyTwo => 0x00,
            SixtyFour => (self.ram_bank & 0x03) << 4,
            OneHundredTwentyEight => (self.ram_bank & 0x03) << 5,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.bank_count),
        }
    }

    fn high_bank(&self) -> u8 {
        use BankCount::*;

        let base = self.rom_bank & self.rom_size_mask();

        match self.bank_count {
            None | Four | Eight | Sixteen | ThirtyTwo => base,
            SixtyFour => base & !(0x01 << 5) | ((self.ram_bank & 0x01) << 5),
            OneHundredTwentyEight => base & !(0x03 << 5) | ((self.ram_bank & 0x03) << 5),
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.bank_count),
        }
    }

    fn rom_size_mask(&self) -> u8 {
        use BankCount::*;

        match self.bank_count {
            None => 0b00000001,
            Four => 0b00000011,
            Eight => 0b00000111,
            Sixteen => 0b00001111,
            ThirtyTwo | SixtyFour | OneHundredTwentyEight => 0b00011111,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.bank_count),
        }
    }

    fn ram_addr(&self, addr: u16) -> u16 {
        use RamSize::*;

        match self.ram_size {
            _2KB | _8KB => (addr - 0xA000) % self.ram_size.len() as u16,
            _32KB => {
                if self.mode {
                    0x2000 * self.ram_bank as u16 + (addr - 0xA000)
                } else {
                    addr - 0xA000
                }
            }
            _ => unreachable!("RAM size can not be greater than 32KB on MBC1"),
        }
    }
}

impl MBCIo for MBC1 {
    fn handle_read(&self, addr: u16) -> MBCResult {
        use MBCResult::*;

        match addr {
            0x0000..=0x3FFF => {
                if self.mode {
                    Address(0x4000 * self.zero_bank() as usize + addr as usize)
                } else {
                    Address(addr as usize)
                }
            }
            0x4000..=0x7FFF => {
                Address(0x4000 * self.high_bank() as usize + (addr as usize - 0x4000))
            }
            0xA000..=0xBFFF => {
                if self.ram_enabled {
                    Value(self.ram[self.ram_addr(addr) as usize])
                } else {
                    Value(0xFF)
                }
            }
            _ => unreachable!("A read from {:#06X} should not be handled by MBC1", addr),
        }
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x1FFF => self.ram_enabled = (byte & 0x0F) == 0x0A,
            0x2000..=0x3FFF => {
                self.rom_bank = if byte == 0x00 {
                    0x01
                } else {
                    byte & self.rom_size_mask()
                };

                self.rom_bank &= 0x1F;
            }
            0x4000..=0x5FFF => self.ram_bank = byte & 0x03,
            0x6000..=0x7FFF => self.mode = (byte & 0x01) == 0x01,
            0xA000..=0xBFFF if self.ram_enabled => {
                let ram_addr = self.ram_addr(addr) as usize;
                self.ram[ram_addr] = byte;
            }
            0xA000..=0xBFFF => {} // Ram isn't enabled, ignored write
            _ => unreachable!("A write to {:#06X} should not be handled by MBC1", addr),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum MBC3Device {
    ExternalRam,
    RealTimeClock,
}

#[derive(Debug, Default)]
struct MBC3 {
    /// 7-bit Number
    rom_bank: u8,
    /// 2-bit Number
    ram_bank: u8,

    devices_enabled: bool,
    currently_mapped: Option<MBC3Device>,
    ram_size: RamSize,
    ram: Vec<u8>,

    // RTC Data Latch Previous Write
    prev_latch_write: Option<u8>,
}

impl MBCIo for MBC3 {
    fn handle_read(&self, addr: u16) -> MBCResult {
        use MBCResult::*;

        let res = match addr {
            0x0000..=0x3FFF => Address(addr as usize),
            0x4000..=0x7FFF => Address(0x4000 * self.rom_bank as usize + (addr as usize - 0x4000)),
            0xA000..=0xBFFF => match self.currently_mapped {
                Some(MBC3Device::ExternalRam) if self.devices_enabled => {
                    Value(self.ram[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)])
                }
                Some(MBC3Device::RealTimeClock) if self.devices_enabled => {
                    todo!("Return Latched value of register")
                }
                _ => Value(0xFF),
            },
            _ => unreachable!("A read from {:#06X} should not be handled by MBC3", addr),
        };

        res
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x000..=0x1FFF => self.devices_enabled = (byte & 0x0F) == 0x0A, // Enable External RAM and Access to RTC if there is one
            0x2000..=0x3FFF => {
                self.rom_bank = match byte {
                    0x00 => 0x01,
                    byte => byte & 0x7F,
                }
            }
            0x4000..=0x5FFF => match byte {
                0x00 | 0x01 | 0x02 | 0x03 => {
                    self.ram_bank = byte & 0x03;
                    self.currently_mapped = Some(MBC3Device::ExternalRam);
                }
                0x08 | 0x09 | 0x0A | 0x0B | 0x0C => {
                    self.currently_mapped = Some(MBC3Device::RealTimeClock);
                }
                _ => {}
            },
            0x6000..=0x7FFF => {
                if let Some(0x00) = self.prev_latch_write {
                    if byte == 0x01 {
                        todo!("Perform Data Latch")
                    }
                }
                self.prev_latch_write = Some(byte);
            }
            0xA000..=0xBFFF => match self.currently_mapped {
                Some(MBC3Device::ExternalRam) if self.devices_enabled => {
                    self.ram[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)] = byte
                }
                Some(MBC3Device::RealTimeClock) if self.devices_enabled => {
                    todo!("Write to RTC")
                }
                _ => {}
            },
            _ => unreachable!("A write to {:#06X} should not be handled by MBC3", addr),
        }
    }
}

#[derive(Debug)]
struct NoMBC {}

impl MBCIo for NoMBC {
    fn handle_read(&self, addr: u16) -> MBCResult {
        MBCResult::Address(addr as usize)
    }

    fn handle_write(&mut self, _addr: u16, _byte: u8) {
        // eprintln!("Tried to write {:#04X} to a read-only cartridge", byte);
    }
}

trait MBCIo {
    fn handle_read(&self, addr: u16) -> MBCResult;
    fn handle_write(&mut self, addr: u16, byte: u8);
}

#[derive(Debug, Clone, Copy)]
enum MBCResult {
    Address(usize),
    Value(u8),
}

#[derive(Debug, Clone, Copy)]
enum MBCKind {
    None,
    MBC1,
    MBC1WithBattery,
    MBC5,
    MBC3WithBattery,
    MBC3,
}

impl Default for MBCKind {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, Copy)]
enum RamSize {
    None = 0x00,
    _2KB = 0x01,
    _8KB = 0x02,
    _32KB = 0x03,  // Split into 4 RAM banks
    _128KB = 0x04, // Split into 16 RAM banks
    _64KB = 0x05,  // Split into 8 RAm Banks
}

impl RamSize {
    fn len(&self) -> u32 {
        use RamSize::*;

        match *self {
            None => 0,
            _2KB => 2_048,
            _8KB => 8_192,
            _32KB => 32_768,
            _128KB => 131_072,
            _64KB => 65_536,
        }
    }
}

impl Default for RamSize {
    fn default() -> Self {
        Self::None
    }
}

impl From<u8> for RamSize {
    fn from(byte: u8) -> Self {
        use RamSize::*;

        match byte {
            0x00 => None,
            0x01 => _2KB,
            0x02 => _8KB,
            0x03 => _32KB,
            0x04 => _128KB,
            0x05 => _64KB,
            _ => unreachable!("{:#04X} is not a valid value for RAMSize", byte),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BankCount {
    None = 0x00,                  // 32KB (also called Two)
    Four = 0x01,                  // 64KB
    Eight = 0x02,                 // 128KB
    Sixteen = 0x03,               // 256KB
    ThirtyTwo = 0x04,             // 512KB
    SixtyFour = 0x05,             // 1MB
    OneHundredTwentyEight = 0x06, // 2MB
    TwoHundredFiftySix = 0x07,    // 4MB
    FiveHundredTwelve = 0x08,     // 8MB
    SeventyTwo = 0x52,            // 1.1MB
    Eighty = 0x53,                // 1.2MB
    NinetySix = 0x54,             // 1.5MB
}

impl Default for BankCount {
    fn default() -> Self {
        Self::None
    }
}

impl BankCount {
    // https://hacktix.github.io/GBEDG/mbcs/#rom-size
    fn size(&self) -> u32 {
        use BankCount::*;

        match self {
            None => 32_768,
            Four => 65_536,
            Eight => 131_072,
            Sixteen => 262_144,
            ThirtyTwo => 524_288,
            SixtyFour => 1_048_576,
            OneHundredTwentyEight => 2_097_152,
            TwoHundredFiftySix => 4_194_304,
            FiveHundredTwelve => 8_388_608,
            SeventyTwo => 1_179_648,
            Eighty => 1_310_720,
            NinetySix => 1_572_864,
        }
    }
}

impl From<u8> for BankCount {
    fn from(byte: u8) -> Self {
        use BankCount::*;

        match byte {
            0x00 => None,
            0x01 => Four,
            0x02 => Eight,
            0x03 => Sixteen,
            0x04 => ThirtyTwo,
            0x05 => SixtyFour,
            0x06 => OneHundredTwentyEight,
            0x07 => TwoHundredFiftySix,
            0x08 => FiveHundredTwelve,
            0x52 => SeventyTwo,
            0x53 => Eighty,
            0x54 => NinetySix,
            _ => unreachable!("{:#04X} is not a valid value for BankCount", byte),
        }
    }
}

impl std::fmt::Debug for Box<dyn MBCIo> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("Implement Debug for Box<dyn MBC> Trait Object");
    }
}

impl Default for Box<dyn MBCIo> {
    fn default() -> Self {
        Box::new(MBC1::default())
    }
}
