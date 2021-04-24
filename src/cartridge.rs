use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

const RAM_SIZE_ADDRESS: usize = 0x0149;
const ROM_SIZE_ADDRESS: usize = 0x0148;
const MBC_TYPE_ADDRESS: usize = 0x0147;

#[derive(Debug, Clone, Default)]
pub struct Cartridge {
    memory: Vec<u8>,
    title: Option<String>,
    mbc: Box<dyn MemoryBankController>,
}

impl Cartridge {
    pub fn new<P: AsRef<Path> + ?Sized>(path: &P) -> io::Result<Self> {
        let mut memory = vec![];
        let mut rom = File::open(path)?;
        rom.read_to_end(&mut memory)?;

        Ok(Self {
            mbc: Self::detect_mbc(&memory),
            title: Self::find_title(&memory),
            memory,
        })
    }

    fn detect_mbc(memory: &[u8]) -> Box<dyn MemoryBankController> {
        let ram_size = Self::find_ram_size(&memory);
        let bank_count = Self::find_bank_count(&memory);
        let mbc_kind = Self::find_mbc(&memory);
        let ram_byte_count = ram_size.as_byte_count();

        match mbc_kind {
            MbcKind::None => Box::new(NoMbc {}),
            MbcKind::Mbc1 => {
                let mbc = Mbc1 {
                    ram_size,
                    ram: vec![0; ram_byte_count as usize],
                    bank_count,
                    ..Default::default()
                };

                Box::new(mbc)
            }
            MbcKind::Mbc5 => todo!("Implement MBC5"),
        }
    }

    fn find_title(memory: &[u8]) -> Option<String> {
        // FIXME: Get rid of magic values and handle cases
        // where 0x134..0x143 reads past the length of the
        // string

        let slice = &memory[0x134..0x143];

        let str_with_nulls = std::str::from_utf8(slice).ok();
        str_with_nulls.map(|s| s.trim_matches('\0').to_string())
    }

    pub fn title(&self) -> Option<&str> {
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

    fn find_mbc(memory: &[u8]) -> MbcKind {
        let id = memory[MBC_TYPE_ADDRESS];

        // TODO: Refactor this to match the other enums in this module
        match id {
            0x00 => MbcKind::None,
            0x01 => MbcKind::Mbc1,
            0x19 => MbcKind::Mbc5,
            _ => unimplemented!("{} is the id of an unsupported memory bank controller", id),
        }
    }
}

impl Cartridge {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match self.mbc.handle_read(addr) {
            MbcResult::Address(addr) => self.memory[addr as usize],
            MbcResult::Value(byte) => byte,
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
struct Mbc1 {
    rom_bank: u8, // 5-bit Number
    ram_bank: u8, // 2-bit number
    mode: bool,
    ram_size: RamSize,
    ram: Vec<u8>,
    bank_count: BankCount,
    ram_enabled: bool,
}

impl Mbc1 {
    fn calc_zero_bank_number(&self) -> u8 {
        use BankCount::*;

        match self.bank_count {
            ThirtyTwo | Sixteen | Eight | Four => 0,
            SixtyFour => (self.ram_bank & 0x01) << 5,
            OneHundredTwentyEight => self.ram_bank << 5,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.bank_count),
        }
    }

    fn calc_high_bank_number(&self) -> u8 {
        use BankCount::*;

        let less_than_mb = self.apply_rom_size_bitmask(self.rom_bank);

        match self.bank_count {
            None | Four | Eight | Sixteen | ThirtyTwo => less_than_mb,
            SixtyFour => (less_than_mb & !(0x01 << 5)) | (self.ram_bank & 0x01) << 5,
            OneHundredTwentyEight => (less_than_mb & !(0b11 << 5)) | (self.ram_bank & 0b11) << 5,
            _ => unreachable!("{:?} is not a valid MBC1 BankCount", self.bank_count),
        }
    }

    fn apply_rom_size_bitmask(&self, byte: u8) -> u8 {
        use BankCount::*;

        let err_bc = self.bank_count; // Bank Count, but with a shorter name

        let result = match self.bank_count {
            None => byte, // FIXME: I don't think this is the correct behaviour
            Four => byte & 0b00000011,
            Eight => byte & 0b00000111,
            Sixteen => byte & 0b00001111,
            ThirtyTwo => byte & 0b00011111,
            SixtyFour => byte & 0b00011111,
            OneHundredTwentyEight => byte & 0b00011111,
            _ => unreachable!("{:?} does not have a rom size bitmask in MBC1", err_bc),
        };

        if result == 0 {
            return 1;
        }
        result
    }

    fn calc_ram_address(&self, addr: u16) -> u16 {
        match self.ram_size {
            RamSize::_2KB | RamSize::_8KB => {
                let ram_size = self.ram_size.as_byte_count() as u16;
                (addr - 0xA000) % ram_size
            }
            RamSize::_32KB => {
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

impl MemoryBankController for Mbc1 {
    fn handle_read(&self, addr: u16) -> MbcResult {
        use MbcResult::*;

        match addr {
            0x0000..=0x3FFF => {
                if self.mode {
                    let zero_bank = self.calc_zero_bank_number() as u16;
                    Address(0x4000 * zero_bank + addr)
                } else {
                    Address(addr)
                }
            }
            0x4000..=0x7FFF => {
                let high_bank = self.calc_high_bank_number() as u16;
                Address(0x4000 * high_bank + (addr - 0x4000))
            }
            0xA000..=0xBFFF => {
                if self.ram_enabled {
                    let ram_addr = self.calc_ram_address(addr);
                    Value(self.ram[ram_addr as usize])
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
                self.rom_bank = self.apply_rom_size_bitmask(byte);
            }
            0x4000..=0x5FFF => self.ram_bank = byte & 0b11,
            0x6000..=0x7FFF => self.mode = (byte & 0x01) == 0x01,
            0xA000..=0xBFFF => {
                if self.ram_enabled {
                    let ram_addr = self.calc_ram_address(addr);
                    self.ram[ram_addr as usize] = byte;
                }
            }
            _ => unreachable!("A write to {:#06X} should not be handled by MBC1", addr),
        }
    }
}
#[derive(Debug, Clone, Copy)]
struct NoMbc {}

impl MemoryBankController for NoMbc {
    fn handle_read(&self, addr: u16) -> MbcResult {
        MbcResult::Address(addr)
    }

    fn handle_write(&mut self, _addr: u16, byte: u8) {
        eprintln!("Tried to write {:#04X} to a read-only cartridge", byte);
    }
}

trait MemoryBankController: CloneMbc {
    fn handle_read(&self, addr: u16) -> MbcResult;
    fn handle_write(&mut self, addr: u16, byte: u8);
}

trait CloneMbc {
    fn clone_mbc(&self) -> Box<dyn MemoryBankController>;
}

impl<T> CloneMbc for T
where
    T: MemoryBankController + Clone + 'static,
{
    fn clone_mbc<'a>(&self) -> Box<dyn MemoryBankController> {
        Box::new(self.clone())
    }
}

enum MbcResult {
    Address(u16),
    Value(u8),
}

#[derive(Debug, Clone, Copy)]
enum MbcKind {
    None,
    Mbc1,
    Mbc5,
}

impl Default for MbcKind {
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
    pub fn as_byte_count(&self) -> u32 {
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
    None = 0x00,                  // 32KB
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
    pub fn to_byte_count(self) -> u32 {
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

impl std::fmt::Debug for Box<dyn MemoryBankController> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("Implement Debug for Box<dyn MBC> Trait Object");
    }
}

impl std::clone::Clone for Box<dyn MemoryBankController> {
    fn clone(&self) -> Self {
        self.clone_mbc()
    }
}

impl Default for Box<dyn MemoryBankController> {
    fn default() -> Self {
        Box::new(Mbc1::default())
    }
}
