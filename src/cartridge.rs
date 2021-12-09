use bitfield::bitfield;

use crate::bus::BusIo;
use crate::emu::SM83_CLOCK_SPEED;
use crate::Cycle;

const RAM_SIZE_ADDRESS: usize = 0x0149;
const ROM_SIZE_ADDRESS: usize = 0x0148;
const MBC_KIND_ADDRESS: usize = 0x0147;
const ROM_TITLE_START: usize = 0x134;
const ROM_TITLE_MAX_SIZE: usize = 16;
const ROM_MANUFACTURER_START: usize = 0x13F;

#[derive(Debug)]
pub(crate) struct Cartridge {
    mem: Vec<u8>,
    pub(crate) title: Option<String>,
    mbc: Box<dyn MbcIo>,
}

impl Cartridge {
    pub(crate) fn new(mem: Vec<u8>) -> Self {
        let title_mem: &[u8; 16] = mem[ROM_TITLE_START..(ROM_TITLE_START + ROM_TITLE_MAX_SIZE)]
            .try_into()
            .expect("coerce slice containing cartridge title from ROM to [u8; 16]");

        let title = Self::detect_title(title_mem);
        let mbc = Self::detect_mbc(&mem);
        tracing::info!("Title: {:?}", title);

        Self { mem, title, mbc }
    }

    pub(crate) fn ext_ram(&self) -> Option<&[u8]> {
        self.mbc.ext_ram()
    }

    pub(crate) fn ext_ram_mut(&mut self) -> Option<&mut [u8]> {
        self.mbc.ext_ram_mut()
    }

    #[inline]
    pub(crate) fn tick(&mut self) {
        self.mbc.tick()
    }

    fn detect_mbc(mem: &[u8]) -> Box<dyn MbcIo> {
        let ram_size: RamSize = mem[RAM_SIZE_ADDRESS].into();
        let rom_size: RomSize = mem[ROM_SIZE_ADDRESS].into();
        let mbc_kind = Self::detect_mbc_kind(mem[MBC_KIND_ADDRESS]);
        let ram_cap = ram_size.capacity();
        let rom_cap = rom_size.capacity();

        tracing::info!("RAM size: {} bytes", ram_cap);
        tracing::info!("ROM size: {} bytes", rom_cap);
        tracing::info!("MBC kind: {:?}", mbc_kind);

        match mbc_kind {
            MbcKind::None => Box::new(NoMbc),
            MbcKind::Mbc1(hw) => Box::new(Mbc1::new(hw, ram_size, rom_size)),
            MbcKind::Mbc2(hw) => Box::new(Mbc2::new(hw, rom_cap)),
            MbcKind::Mbc3(hw @ Mbc3Hardware::Rtc) => Box::new(Mbc3::new(hw, ram_cap)),
            MbcKind::Mbc3(hw @ Mbc3Hardware::RtcBatteryRam) => Box::new(Mbc3::new(hw, ram_cap)),
            MbcKind::Mbc3(hw @ Mbc3Hardware::BatteryRam) => Box::new(Mbc3::new(hw, ram_cap)),
            MbcKind::Mbc5(hw @ Mbc5Hardware::None) => Box::new(Mbc5::new(hw, ram_cap, rom_cap)),
            MbcKind::Mbc5(hw @ Mbc5Hardware::BatteryRam) => {
                Box::new(Mbc5::new(hw, ram_cap, rom_cap))
            }
            kind => todo!("ROMS with {:?} are currently unsupported", kind),
        }
    }

    fn detect_title(title_mem: &[u8; ROM_TITLE_MAX_SIZE]) -> Option<String> {
        use std::str::from_utf8;

        const ALT_TITLE_LEN: usize = ROM_MANUFACTURER_START - ROM_TITLE_START;

        // ascii byte slie does not have a null-terminator
        let ascii = match title_mem.iter().position(|b| *b == 0x00) {
            Some(end) => &title_mem[..end],
            None => &title_mem[..ROM_TITLE_MAX_SIZE],
        };

        match from_utf8(ascii).map(str::trim).ok() {
            None => match from_utf8(&title_mem[..ALT_TITLE_LEN]).map(str::trim).ok() {
                Some("") | None => None,
                Some(title) => Some(String::from(title)),
            },
            Some("") => None,
            Some(title) => Some(String::from(title)),
        }
    }

    fn detect_mbc_kind(id: u8) -> MbcKind {
        use MbcKind::*;

        match id {
            0x00 => None,
            0x01 => Mbc1(Mbc1Hardware::None),
            0x02 => Mbc1(Mbc1Hardware::Ram),
            0x03 => Mbc1(Mbc1Hardware::BatteryRam),
            0x05 => Mbc2(Mbc2Hardware::None),
            0x06 => Mbc2(Mbc2Hardware::BatteryRam),
            0x08 | 0x09 => unimplemented!("NoMBC + RAM and NoMBC + Battery unsupported"),
            0x0B | 0x0C | 0x0D => unimplemented!("MM01 unsupported"),
            0x0F => Mbc3(Mbc3Hardware::Rtc),
            0x10 => Mbc3(Mbc3Hardware::RtcBatteryRam),
            0x11 => Mbc3(Mbc3Hardware::None),
            0x12 => Mbc3(Mbc3Hardware::Ram),
            0x13 => Mbc3(Mbc3Hardware::BatteryRam),
            0x19 => Mbc5(Mbc5Hardware::None),
            0x1A => Mbc5(Mbc5Hardware::Ram),
            0x1B => Mbc5(Mbc5Hardware::BatteryRam),
            0x1C => Mbc5(Mbc5Hardware::Rumble),
            0x1D => Mbc5(Mbc5Hardware::RumbleRam),
            0x1E => Mbc5(Mbc5Hardware::RumbleBatteryRam),
            id => unimplemented!("MBC with code {:#04X} is unsupported", id),
        }
    }
}

impl BusIo for Cartridge {
    fn read_byte(&self, addr: u16) -> u8 {
        use MbcResult::*;

        match self.mbc.handle_read(addr) {
            Addr(addr) => self.mem[addr],
            Byte(byte) => byte,
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.mbc.handle_write(addr, byte);
    }
}

#[derive(Debug)]
struct Mbc1 {
    /// 5-bit number
    rom_bank: u8,
    /// 2-bit number
    ram_bank: u8,
    mode: bool,
    ram_size: RamSize,
    mem: Vec<u8>,
    rom_size: RomSize,
    mem_enabled: bool,

    hw: Mbc1Hardware,
}

impl Mbc1 {
    fn new(hw: Mbc1Hardware, ram_size: RamSize, rom_size: RomSize) -> Self {
        Self {
            rom_bank: 0x01,
            mem: vec![0; ram_size.capacity() as usize],
            ram_size,
            rom_size,
            ram_bank: Default::default(),
            mode: Default::default(),
            mem_enabled: Default::default(),
            hw,
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

    fn ram_addr(&self, addr: u16) -> usize {
        use RamSize::*;

        match self.ram_size {
            Unused | One => (addr as usize - 0xA000) % self.ram_size.capacity(),
            Four => {
                if self.mode {
                    0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)
                } else {
                    addr as usize - 0xA000
                }
            }
            _ => unreachable!("RAM size can not be greater than 32KB on MBC1"),
        }
    }
}

impl Savable for Mbc1 {
    fn ext_ram(&self) -> Option<&[u8]> {
        match self.hw {
            Mbc1Hardware::BatteryRam => Some(&self.mem),
            _ => None,
        }
    }

    fn ext_ram_mut(&mut self) -> Option<&mut [u8]> {
        match self.hw {
            Mbc1Hardware::BatteryRam => Some(&mut self.mem),
            _ => None,
        }
    }
}

impl MbcIo for Mbc1 {
    fn handle_read(&self, addr: u16) -> MbcResult {
        use MbcResult::*;

        match addr {
            0x0000..=0x3FFF if self.mode => {
                Addr(0x4000 * self.zero_bank() as usize + addr as usize)
            }
            0x0000..=0x3FFF => Addr(addr as usize),
            0x4000..=0x7FFF => Addr(0x4000 * self.high_bank() as usize + (addr as usize - 0x4000)),
            0xA000..=0xBFFF if self.mem_enabled => Byte(self.mem[self.ram_addr(addr)]),
            0xA000..=0xBFFF => Byte(0xFF),
            _ => unreachable!("A read from {:#06X} should not be handled by MBC1", addr),
        }
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x1FFF => self.mem_enabled = (byte & 0x0F) == 0x0A,
            0x2000..=0x3FFF => {
                let value = byte & 0x1F;
                let masked_value = byte & self.rom_size_mask();
                self.rom_bank = if value == 0 { 0x01 } else { masked_value };
            }
            0x4000..=0x5FFF => self.ram_bank = byte & 0x03,
            0x6000..=0x7FFF => self.mode = (byte & 0x01) == 0x01,
            0xA000..=0xBFFF if self.mem_enabled => {
                let ram_addr = self.ram_addr(addr);
                self.mem[ram_addr] = byte;
            }
            0xA000..=0xBFFF => {} // Ram isn't enabled, ignored write
            _ => unreachable!("A write to {:#06X} should not be handled by MBC1", addr),
        }
    }
}

impl RtcTick for Mbc1 {
    fn tick(&mut self) {}
}

#[derive(Debug, Default, Clone, Copy)]
struct RtClock {
    /// 6-bit unsigned integer
    sec: u8,
    /// 6-bit unsigned integer
    min: u8,
    /// 6-bit unsigned integer
    hr: u8,
    day_low: u8,
    day_high: DayHigh,

    cycles: Cycle,
}

impl RtClock {
    fn inc_day(&mut self) {
        // TODO: Figure out order of operations, the brackets are a bit too defensive here
        let days: u16 = (((self.day_high.ninth() as u16) << 8) | self.day_low as u16) + 1;

        if days > 0x1FF {
            self.day_high.set_carry(true);
        }

        self.day_high.set_ninth(((days >> 8) & 0x01) == 0x01);
        self.day_low = days as u8;
    }
}

impl RtcTick for RtClock {
    fn tick(&mut self) {
        // This is the sort of situation where you'd want to use a scheduler.
        if self.day_high.halt() {
            return;
        }

        self.cycles += 1;

        if self.cycles >= SM83_CLOCK_SPEED {
            self.cycles %= SM83_CLOCK_SPEED;

            self.sec += 1;

            if self.sec == 60 {
                self.sec = 0;
                self.min += 1;
            }

            if self.min == 60 {
                self.min = 0;
                self.hr += 1;
            }

            if self.hr == 24 {
                self.hr = 0;
                self.inc_day();
            }
        }
    }
}

trait RtcTick {
    fn tick(&mut self);
}

bitfield! {
    struct DayHigh(u8);
    impl Debug;
    _, set_carry: 7;
    halt, _: 6;
    ninth, set_ninth: 0;
}

impl Copy for DayHigh {}
impl Clone for DayHigh {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for DayHigh {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for DayHigh {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<DayHigh> for u8 {
    fn from(dh: DayHigh) -> Self {
        dh.0
    }
}

#[derive(Debug, Clone, Copy)]
enum Mbc3Device {
    Ram,
    Rtc(RtcRegister),
}

#[derive(Debug, Clone, Copy)]
enum RtcRegister {
    Sec,
    Min,
    Hr,
    DayLow,
    DayHigh,
}

#[derive(Debug)]
struct Mbc3 {
    /// 7-bit Number
    rom_bank: u8,
    /// 2-bit Number
    ram_bank: u8,

    devs_enabled: bool,
    mapped: Option<Mbc3Device>,
    mem: Vec<u8>,

    // RTC Data Latch Previous Write
    prev_latch_write: Option<u8>,

    hw: Mbc3Hardware,
    rtc: RtClock,
    rtc_latch: Option<RtClock>,
}

impl Mbc3 {
    fn new(hw: Mbc3Hardware, ram_cap: usize) -> Self {
        Self {
            mem: vec![0; ram_cap],
            rom_bank: Default::default(),
            ram_bank: Default::default(),
            devs_enabled: Default::default(),
            mapped: Default::default(),
            prev_latch_write: Default::default(),
            rtc: Default::default(),
            rtc_latch: Default::default(),
            hw,
        }
    }
}

impl Savable for Mbc3 {
    fn ext_ram(&self) -> Option<&[u8]> {
        match self.hw {
            Mbc3Hardware::BatteryRam | Mbc3Hardware::RtcBatteryRam => Some(&self.mem),
            _ => None,
        }
    }

    fn ext_ram_mut(&mut self) -> Option<&mut [u8]> {
        match self.hw {
            Mbc3Hardware::BatteryRam | Mbc3Hardware::RtcBatteryRam => Some(&mut self.mem),
            _ => None,
        }
    }
}

impl MbcIo for Mbc3 {
    fn handle_read(&self, addr: u16) -> MbcResult {
        use MbcResult::*;
        use RtcRegister::*;

        let res = match addr {
            0x0000..=0x3FFF => Addr(addr as usize),
            0x4000..=0x7FFF => Addr(0x4000 * self.rom_bank as usize + (addr as usize - 0x4000)),
            0xA000..=0xBFFF => match self.mapped {
                Some(Mbc3Device::Ram) if self.devs_enabled => {
                    Byte(self.mem[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)])
                }
                Some(Mbc3Device::Rtc(reg)) if self.devs_enabled => Byte(
                    self.rtc_latch
                        .as_ref()
                        .map(|rtc| match reg {
                            Sec => rtc.sec,
                            Min => rtc.min,
                            Hr => rtc.hr,
                            DayLow => rtc.day_low,
                            DayHigh => rtc.day_high.into(),
                        })
                        .unwrap_or(0xFF),
                ),
                _ => Byte(0xFF),
            },
            _ => unreachable!("A read from {:#06X} should not be handled by MBC3", addr),
        };

        res
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        use RtcRegister::*;

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
                    self.mapped = Some(Mbc3Device::Ram);
                }
                0x08 => self.mapped = Some(Mbc3Device::Rtc(Sec)),
                0x09 => self.mapped = Some(Mbc3Device::Rtc(Min)),
                0x0A => self.mapped = Some(Mbc3Device::Rtc(Hr)),
                0x0B => self.mapped = Some(Mbc3Device::Rtc(DayLow)),
                0x0C => self.mapped = Some(Mbc3Device::Rtc(DayHigh)),

                _ => {}
            },
            0x6000..=0x7FFF => {
                if let Some(0x00) = self.prev_latch_write {
                    if byte == 0x01 {
                        self.rtc_latch = Some(self.rtc);
                    }
                }
                self.prev_latch_write = Some(byte);
            }
            0xA000..=0xBFFF => match self.mapped {
                Some(Mbc3Device::Ram) if self.devs_enabled => {
                    self.mem[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)] = byte
                }
                Some(Mbc3Device::Rtc(rtc_reg)) if self.devs_enabled => match rtc_reg {
                    Sec => {
                        self.rtc.sec = byte & 0x3F;
                        // Writing to RTC S resets the internal sub-second counter
                        self.rtc.cycles = 0;
                    }
                    Min => self.rtc.min = byte & 0x3F,
                    Hr => self.rtc.hr = byte & 0x1F,
                    DayLow => self.rtc.day_low = byte,
                    DayHigh => self.rtc.day_high = (byte & 0xC1).into(),
                },
                _ => {}
            },
            _ => unreachable!("A write to {:#06X} should not be handled by MBC3", addr),
        }
    }
}

impl RtcTick for Mbc3 {
    fn tick(&mut self) {
        if let Mbc3Hardware::RtcBatteryRam | Mbc3Hardware::Rtc = self.hw {
            self.rtc.tick();
        }
    }
}

#[derive(Debug)]
struct Mbc5 {
    /// 9-bit number
    rom_bank: u16,
    /// 4-bit number
    ram_bank: u8,

    rom_cap: usize,
    mem: Vec<u8>,
    mem_enabled: bool,

    hw: Mbc5Hardware,
}

impl Mbc5 {
    fn new(hw: Mbc5Hardware, ram_cap: usize, rom_cap: usize) -> Self {
        Self {
            rom_bank: 0x01,
            mem: vec![0; ram_cap],
            rom_cap,
            ram_bank: Default::default(),
            mem_enabled: Default::default(),
            hw,
        }
    }

    fn bank_addr(&self, addr: u16) -> usize {
        (0x4000 * self.rom_bank as usize + (addr as usize - 0x4000)) % self.rom_cap
    }
}

impl Savable for Mbc5 {
    fn ext_ram(&self) -> Option<&[u8]> {
        match self.hw {
            Mbc5Hardware::RumbleBatteryRam | Mbc5Hardware::BatteryRam => Some(&self.mem),
            _ => None,
        }
    }

    fn ext_ram_mut(&mut self) -> Option<&mut [u8]> {
        match self.hw {
            Mbc5Hardware::RumbleBatteryRam | Mbc5Hardware::BatteryRam => Some(&mut self.mem),
            _ => None,
        }
    }
}

impl MbcIo for Mbc5 {
    fn handle_read(&self, addr: u16) -> MbcResult {
        use MbcResult::*;

        match addr {
            0x0000..=0x3FFF => Addr(addr as usize),
            0x4000..=0x7FFF => Addr(self.bank_addr(addr)),
            0xA000..=0xBFFF if self.mem_enabled => {
                Byte(self.mem[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)])
            }
            0xA000..=0xBFFF => Byte(0xFF),
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
                self.mem[0x2000 * self.ram_bank as usize + (addr as usize - 0xA000)] = byte;
            }
            0xA000..=0xBFFF => {}
            _ => unreachable!("A write to {:#06X} should not be handled by MBC5", addr),
        }
    }
}

impl RtcTick for Mbc5 {
    fn tick(&mut self) {}
}

#[derive(Debug)]
struct Mbc2 {
    /// 4-bit number
    rom_bank: u8,
    mem: Box<[u8; Self::RAM_SIZE]>,

    mem_enabled: bool,
    rom_cap: usize,
    hw: Mbc2Hardware,
}

impl Mbc2 {
    const RAM_SIZE: usize = 0x0200;

    fn new(hw: Mbc2Hardware, rom_cap: usize) -> Self {
        Self {
            rom_bank: 0x01,
            mem: Box::new([0; Self::RAM_SIZE]),
            rom_cap,
            mem_enabled: Default::default(),
            hw,
        }
    }

    fn rom_addr(&self, addr: u16) -> usize {
        (0x4000 * self.rom_bank as usize + (addr as usize - 0x4000)) % self.rom_cap
    }
}

impl Savable for Mbc2 {
    fn ext_ram(&self) -> Option<&[u8]> {
        match self.hw {
            Mbc2Hardware::BatteryRam => Some(self.mem.as_ref()),
            Mbc2Hardware::None => None,
        }
    }

    fn ext_ram_mut(&mut self) -> Option<&mut [u8]> {
        match self.hw {
            Mbc2Hardware::BatteryRam => Some(self.mem.as_mut()),
            Mbc2Hardware::None => None,
        }
    }
}

impl MbcIo for Mbc2 {
    fn handle_read(&self, addr: u16) -> MbcResult {
        use MbcResult::*;

        match addr {
            0x0000..=0x3FFF => Addr(addr as usize),
            0x4000..=0x7FFF => Addr(self.rom_addr(addr)),
            0xA000..=0xBFFF if self.mem_enabled => {
                let mbc2_addr = addr as usize & (Self::RAM_SIZE - 1);
                Byte(self.mem[mbc2_addr] | 0xF0)
            }
            0xA000..=0xBFFF => Byte(0xFF),
            _ => unreachable!("A read from {:#06X} should not be handled by MBC2", addr),
        }
    }

    fn handle_write(&mut self, addr: u16, byte: u8) {
        let nybble = byte & 0x0F;

        match addr {
            0x0000..=0x3FFF if addr >> 8 & 0x01 == 0x01 => {
                self.rom_bank = if nybble == 0x00 { 0x01 } else { nybble };
            }
            0x0000..=0x3FFF => self.mem_enabled = nybble == 0x0A,
            0xA000..=0xBFFF if self.mem_enabled => {
                let mbc2_addr = addr as usize & (Self::RAM_SIZE - 1);
                self.mem[mbc2_addr] = nybble;
            }
            0x4000..=0x7FFF | 0xA000..=0xBFFF => {}
            _ => unreachable!("A write to {:#06X} should not be handled by MBC2", addr),
        }
    }
}

impl RtcTick for Mbc2 {
    fn tick(&mut self) {}
}

#[derive(Debug)]
struct NoMbc;

impl Savable for NoMbc {
    fn ext_ram(&self) -> Option<&[u8]> {
        None
    }

    fn ext_ram_mut(&mut self) -> Option<&mut [u8]> {
        None
    }
}

impl MbcIo for NoMbc {
    fn handle_read(&self, addr: u16) -> MbcResult {
        MbcResult::Addr(addr as usize)
    }

    fn handle_write(&mut self, _: u16, byte: u8) {
        tracing::warn!("Attempted write of {:#04X} to cartridge w/out MBC", byte);
    }
}

impl RtcTick for NoMbc {
    fn tick(&mut self) {}
}

trait MbcIo: Savable + RtcTick {
    fn handle_read(&self, addr: u16) -> MbcResult;
    fn handle_write(&mut self, addr: u16, byte: u8);
}

#[derive(Debug, Clone, Copy)]
enum MbcResult {
    Addr(usize),
    Byte(u8),
}

#[derive(Debug, Clone, Copy)]
enum MbcKind {
    None,
    Mbc1(Mbc1Hardware),
    Mbc2(Mbc2Hardware),
    Mbc3(Mbc3Hardware),
    Mbc5(Mbc5Hardware),
}

#[derive(Debug, Clone, Copy)]
enum Mbc1Hardware {
    None,
    Ram,
    BatteryRam,
}

#[derive(Debug, Clone, Copy)]
enum Mbc2Hardware {
    None,
    BatteryRam,
}

#[derive(Debug, Clone, Copy)]
enum Mbc3Hardware {
    Rtc,
    RtcBatteryRam,
    None,
    Ram,
    BatteryRam,
}

#[derive(Debug, Clone, Copy)]
enum Mbc5Hardware {
    None,
    Ram,
    BatteryRam,
    Rumble,
    RumbleRam,
    RumbleBatteryRam,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum RamSize {
    None = 0x00,
    Unused = 0x01,
    One = 0x02,
    Four = 0x03,
    Sixteen = 0x04,
    Eight = 0x05,
}

impl RamSize {
    fn capacity(&self) -> usize {
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
    fn capacity(&self) -> usize {
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

impl std::fmt::Debug for Box<dyn MbcIo> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("Implement Debug for Box<dyn MBC> Trait Object");
    }
}

trait Savable {
    fn ext_ram(&self) -> Option<&[u8]>;
    fn ext_ram_mut(&mut self) -> Option<&mut [u8]>;
}

#[cfg(test)]
mod tests {
    use super::Cartridge;

    #[test]
    fn empty_rom_title() {
        let title = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];

        assert_eq!(None, Cartridge::detect_title(&title));
    }

    #[test]
    fn normal_rom_title() {
        let title = [
            0x50, 0x4F, 0x4B, 0x45, 0x4D, 0x4F, 0x4E, 0x20, 0x42, 0x4C, 0x55, 0x45, 0x00, 0x00,
            0x00, 0x00,
        ];

        assert_eq!(
            Some(String::from("POKEMON BLUE")),
            Cartridge::detect_title(&title)
        );
    }

    #[test]
    fn extra_spaces_title() {
        let title = [
            0x54, 0x4f, 0x4b, 0x49, 0x4d, 0x45, 0x4b, 0x49, 0x20, 0x43, 0x55, 0x4c, 0x20, 0x20, 0,
            0,
        ];

        assert_eq!(
            Some(String::from("TOKIMEKI CUL")),
            Cartridge::detect_title(&title)
        )
    }

    #[test]
    fn long_title() {
        let title = [
            0x54, 0x4f, 0x4b, 0x49, 0x4d, 0x45, 0x4b, 0x49, 0x20, 0x43, 0x55, 0x4c, 0x54, 0x55,
            0x52, 0x45,
        ];

        assert_eq!(
            Some(String::from("TOKIMEKI CULTURE")),
            Cartridge::detect_title(&title),
        );
    }

    #[test]
    fn publisher_code_and_title() {
        let title: [u8; 16] = [
            0x47, 0x52, 0x41, 0x4E, 0x44, 0x20, 0x54, 0x48, 0x45, 0x46, 0x54, 0x41, 0x4F, 0x41,
            0x45, 0x80,
        ];

        assert_eq!(
            Some(String::from("GRAND THEFT")),
            Cartridge::detect_title(&title)
        );
    }

    #[test]
    fn all_whitespace_title() {
        let title = [
            0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];

        assert_eq!(None, Cartridge::detect_title(&title));
    }
}
