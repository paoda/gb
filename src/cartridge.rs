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
        let ram_info = Self::detect_ram_info(memory);
        let rom_info = Self::detect_rom_info(memory);
        let mbc_kind = Self::find_mbc(memory);
        let ram_size = ram_info.size();

        eprintln!("Cartridge Ram Size: {} bytes", ram_size);
        eprintln!("Cartridge ROM Size: {} bytes", rom_info.size());
        eprintln!("MBC Type: {:?}", mbc_kind);

        match mbc_kind {
            MBCKind::None => Box::new(NoMBC),
            MBCKind::MBC1 => Box::new(MBC1::new(ram_info, rom_info)),
            MBCKind::MBC1WithBattery => Box::new(MBC1::new(ram_info, rom_info)), // TODO: Implement Saving
            MBCKind::MBC3 => Box::new(MBC3::new(ram_size)),
            MBCKind::MBC3WithBattery => Box::new(MBC3::new(ram_size)), // TODO: Implement Saving
            MBCKind::MBC5 => Box::new(MBC5::new(ram_size)),
            MBCKind::MBC5WithBattery => Box::new(MBC5::new(ram_size)), // TDO: Implement Saving
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

    fn detect_ram_info(memory: &[u8]) -> RamSize {
        let id = memory[RAM_SIZE_ADDRESS];
        id.into()
    }

    fn detect_rom_info(memory: &[u8]) -> RomSize {
        let id = dbg!(memory[ROM_SIZE_ADDRESS]);
        id.into()
    }

    fn find_mbc(memory: &[u8]) -> MBCKind {
        match memory[MBC_TYPE_ADDRESS] {
            0x00 => MBCKind::None,
            0x01 | 0x02 => MBCKind::MBC1,
            0x03 => MBCKind::MBC1WithBattery,
            0x19 | 0x1A => MBCKind::MBC5,
            0x1B => MBCKind::MBC5WithBattery,
            0x13 => MBCKind::MBC3WithBattery,
            0x11 | 0x12 => MBCKind::MBC3,
            id => unimplemented!("id {:#04X} is an unsupported MBC", id),
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
    memory: Vec<u8>,
    rom_size: RomSize,
    mem_enabled: bool,
}

impl MBC1 {
    fn new(ram_size: RamSize, rom_size: RomSize) -> Self {
        Self {
            rom_bank: 0x01,
            memory: vec![0; ram_size.size() as usize],
            ram_size,
            rom_size,
            ram_bank: Default::default(),
            mode: Default::default(),
            mem_enabled: Default::default(),
        }
    }

    fn zero_bank(&self) -> u8 {
        use RomSize::*;

        match self.rom_size {
            None | Four | Eight | Sixteen | ThirtyTwo => 0x00,
            SixtyFour => (self.ram_bank & 0x01) << 5,
            OneTwentyEight => (self.ram_bank & 0x03) << 5,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.rom_size),
        }
    }

    fn _mbcm_zero_bank(&self) -> u8 {
        use RomSize::*;

        match self.rom_size {
            None | Four | Eight | Sixteen | ThirtyTwo => 0x00,
            SixtyFour => (self.ram_bank & 0x03) << 4,
            OneTwentyEight => (self.ram_bank & 0x03) << 5,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.rom_size),
        }
    }

    fn high_bank(&self) -> u8 {
        use RomSize::*;

        let base = self.rom_bank & self.rom_size_mask();

        match self.rom_size {
            None | Four | Eight | Sixteen | ThirtyTwo => base,
            SixtyFour => base & !(0x01 << 5) | ((self.ram_bank & 0x01) << 5),
            OneTwentyEight => base & !(0x03 << 5) | ((self.ram_bank & 0x03) << 5),
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.rom_size),
        }
    }

    fn rom_size_mask(&self) -> u8 {
        use RomSize::*;

        match self.rom_size {
            None => 0b00000001,
            Four => 0b00000011,
            Eight => 0b00000111,
            Sixteen => 0b00001111,
            ThirtyTwo | SixtyFour | OneTwentyEight => 0b00011111,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.rom_size),
        }
    }

    fn ram_addr(&self, addr: u16) -> u16 {
        use RamSize::*;

        match self.ram_size {
            Unused | One => (addr - 0xA000) % self.ram_size.size() as u16,
            Four => {
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
                if self.mem_enabled {
                    Value(self.memory[self.ram_addr(addr) as usize])
                } else {
                    Value(0xFF)
                }
            }
            _ => unreachable!("A read from {:#06X} should not be handled by MBC1", addr),
        }
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x1FFF => self.mem_enabled = (byte & 0x0F) == 0x0A,
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
            0xA000..=0xBFFF if self.mem_enabled => {
                let ram_addr = self.ram_addr(addr) as usize;
                self.memory[ram_addr] = byte;
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

#[derive(Debug)]
struct MBC3 {
    /// 7-bit Number
    rom_bank: u8,
    /// 2-bit Number
    ram_bank: u8,

    devs_enabled: bool,
    mapped: Option<MBC3Device>,
    memory: Vec<u8>,

    // RTC Data Latch Previous Write
    prev_latch_write: Option<u8>,
}

impl MBC3 {
    fn new(ram_size: usize) -> Self {
        Self {
            memory: vec![0; ram_size],
            rom_bank: Default::default(),
            ram_bank: Default::default(),
            devs_enabled: Default::default(),
            mapped: Default::default(),
            prev_latch_write: Default::default(),
        }
    }
}

impl MBCIo for MBC3 {
    fn handle_read(&self, addr: u16) -> MBCResult {
        use MBCResult::*;

        let res = match addr {
            0x0000..=0x3FFF => Address(addr as usize),
            0x4000..=0x7FFF => Address(0x4000 * self.rom_bank as usize + (addr as usize - 0x4000)),
            0xA000..=0xBFFF => match self.mapped {
                Some(MBC3Device::ExternalRam) if self.devs_enabled => {
                    Value(self.memory[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)])
                }
                Some(MBC3Device::RealTimeClock) if self.devs_enabled => {
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
            0x000..=0x1FFF => self.devs_enabled = (byte & 0x0F) == 0x0A, // Enable External RAM and Access to RTC if there is one
            0x2000..=0x3FFF => {
                self.rom_bank = match byte {
                    0x00 => 0x01,
                    byte => byte & 0x7F,
                }
            }
            0x4000..=0x5FFF => match byte {
                0x00 | 0x01 | 0x02 | 0x03 => {
                    self.ram_bank = byte & 0x03;
                    self.mapped = Some(MBC3Device::ExternalRam);
                }
                0x08 | 0x09 | 0x0A | 0x0B | 0x0C => {
                    self.mapped = Some(MBC3Device::RealTimeClock);
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
            0xA000..=0xBFFF => match self.mapped {
                Some(MBC3Device::ExternalRam) if self.devs_enabled => {
                    self.memory[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)] = byte
                }
                Some(MBC3Device::RealTimeClock) if self.devs_enabled => {
                    todo!("Write to RTC")
                }
                _ => {}
            },
            _ => unreachable!("A write to {:#06X} should not be handled by MBC3", addr),
        }
    }
}

#[derive(Debug)]
struct MBC5 {
    /// 9-bit number
    rom_bank: u16,
    /// 4-bit number
    ram_bank: u8,

    memory: Vec<u8>,
    mem_enabled: bool,
}

impl MBC5 {
    fn new(ram_size: usize) -> Self {
        Self {
            rom_bank: 0x01,
            memory: vec![0; ram_size],
            ram_bank: Default::default(),
            mem_enabled: Default::default(),
        }
    }
}

impl MBCIo for MBC5 {
    fn handle_read(&self, addr: u16) -> MBCResult {
        use MBCResult::*;

        match addr {
            0x0000..=0x3FFF => Address(addr as usize),
            0x4000..=0x7FFF => {
                Address(0x4000u16.wrapping_mul(self.rom_bank) as usize + (addr as usize - 0x4000))
            }
            0xA000..=0xBFFF if self.mem_enabled => {
                Value(self.memory[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)])
            }
            0xA000..=0xBFFF => Value(0xFF),
            _ => unreachable!("A read from {:#06X} should not be handled by MBC5", addr),
        }
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x1FFF => self.mem_enabled = (byte & 0x0F) == 0x0A,
            0x2000..=0x2FFF => self.rom_bank = (self.rom_bank & 0x0100) | byte as u16,
            0x3000..=0x3FFF => self.rom_bank = (self.rom_bank & 0x00FF) | (byte as u16 & 0x01) << 8,
            0x4000..=0x5FFF => self.ram_bank = byte & 0x0F,
            0xA000..=0xBFFF if self.mem_enabled => {
                self.memory[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)] = byte;
            }
            0xA000..=0xBFFF => {}
            _ => unreachable!("A write to {:#06X} should not be handled by MBC5", addr),
        }
    }
}

#[derive(Debug)]
struct NoMBC;

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
    MBC3,
    MBC3WithBattery,
    MBC5,
    MBC5WithBattery,
}

impl Default for MBCKind {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, Copy)]
enum RamSize {
    None = 0x00,
    Unused = 0x01,
    One = 0x02,
    Four = 0x03,
    Sixteen = 0x04,
    Eight = 0x05,
}

impl RamSize {
    fn size(&self) -> usize {
        use RamSize::*;

        match *self {
            None => 0,
            Unused => 0x800,
            One => 0x2000,
            Four => 0x8000,
            Sixteen => 0x20000,
            Eight => 0x10000,
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
            0x01 => Unused,
            0x02 => One,
            0x03 => Four,
            0x04 => Sixteen,
            0x05 => Eight,
            _ => unreachable!("{:#04X} is not a valid value for RAMSize", byte),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum RomSize {
    None = 0x00,              // 32KB (also called Two)
    Four = 0x01,              // 64KB
    Eight = 0x02,             // 128KB
    Sixteen = 0x03,           // 256KB
    ThirtyTwo = 0x04,         // 512KB
    SixtyFour = 0x05,         // 1MB
    OneTwentyEight = 0x06,    // 2MB
    TwoFiftySix = 0x07,       // 4MB
    FiveHundredTwelve = 0x08, // 8MB
    SeventyTwo = 0x52,        // 1.1MB
    Eighty = 0x53,            // 1.2MB
    NinetySix = 0x54,         // 1.5MB
}

impl RomSize {
    // https://hacktix.github.io/GBEDG/mbcs/#rom-size
    fn size(&self) -> u32 {
        use RomSize::*;

        match self {
            SeventyTwo => 0x120000,
            Eighty => 0x140000,
            NinetySix => 0x180000,
            _ => 0x8000 << *self as u8,
        }
    }
}

impl From<u8> for RomSize {
    fn from(byte: u8) -> Self {
        use RomSize::*;

        match byte {
            0x00 => None,
            0x01 => Four,
            0x02 => Eight,
            0x03 => Sixteen,
            0x04 => ThirtyTwo,
            0x05 => SixtyFour,
            0x06 => OneTwentyEight,
            0x07 => TwoFiftySix,
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
        Box::new(NoMBC)
    }
}
