use crate::Cycle;
use crate::GB_HEIGHT;
use crate::GB_WIDTH;
use bitfield::bitfield;
use std::collections::VecDeque;
use std::convert::TryInto;

const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;
const PPU_START_ADDRESS: usize = 0x8000;

// OAM Scan
const SPRITE_BUFFER_LIMIT: usize = 10;

const WHITE: [u8; 4] = [0xFF, 0xFF, 0xFF, 0xFF];
const LIGHT_GRAY: [u8; 4] = [0xCC, 0xCC, 0xCC, 0xFF];
const DARK_GRAY: [u8; 4] = [0x77, 0x77, 0x77, 0xFF];
const BLACK: [u8; 4] = [0x00, 0x00, 0x00, 0x00];

#[derive(Debug, Clone)]
pub struct Ppu {
    pub int: Interrupt,
    pub control: LCDControl,
    pub monochrome: Monochrome,
    pub pos: ScreenPosition,
    pub vram: Box<[u8; VRAM_SIZE]>,
    pub stat: LCDStatus,
    pub oam: SpriteAttributeTable,
    fetcher: PixelFetcher,
    fifo: FifoRenderer,
    sprite_buffer: SpriteBuffer,
    frame_buf: Box<[u8; GB_WIDTH * GB_HEIGHT * 4]>,
    x_pos: u8,
    cycles: Cycle,
}

impl Ppu {
    pub fn read_byte(&self, addr: u16) -> u8 {
        self.vram[addr as usize - PPU_START_ADDRESS]
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        self.vram[addr as usize - PPU_START_ADDRESS] = byte;
    }
}

impl Ppu {
    pub fn step(&mut self, cycles: Cycle) {
        let start: u32 = self.cycles.into();
        let end: u32 = cycles.into();

        for cycle in start..(start + end).into() {
            self.cycles += 1;

            match self.stat.mode() {
                Mode::OamScan => {
                    if self.cycles >= 80.into() {
                        self.stat.set_mode(Mode::Drawing);
                    }

                    self.scan_oam(self.cycles.into());
                }
                Mode::Drawing => {
                    if self.x_pos >= 160 {
                        if self.stat.hblank_int() {
                            // Enable HBlank LCDStat Interrupt
                            self.int.set_lcd_stat(true);
                        }

                        // Done with rendering this frame,
                        // we can reset the ppu x_pos and fetcher state now
                        self.x_pos = 0;
                        self.fetcher.hblank_reset();

                        self.stat.set_mode(Mode::HBlank);
                    } else {
                        self.draw(self.cycles.into());
                    }
                }
                Mode::HBlank => {
                    // This mode will always end at 456 cycles

                    if self.cycles >= 456.into() {
                        self.cycles %= 456;
                        self.pos.line_y += 1;

                        // New Scanline is next, check for LYC=LY
                        if self.stat.coincidence_int() {
                            let are_equal = self.pos.line_y == self.pos.ly_compare;
                            self.stat.set_coincidence(are_equal);
                        }

                        let next_mode = if self.pos.line_y >= 144 {
                            // Request VBlank Interrupt
                            self.int.set_vblank(true);

                            // Reset Window Line Counter in Fetcher
                            self.fetcher.vblank_reset();

                            if self.stat.vblank_int() {
                                // Enable Vblank LCDStat Interrupt
                                self.int.set_lcd_stat(true);
                            }

                            Mode::VBlank
                        } else {
                            if self.stat.oam_int() {
                                // Enable OAM LCDStat Interrupt
                                self.int.set_lcd_stat(true);
                            }

                            Mode::OamScan
                        };

                        self.stat.set_mode(next_mode);
                    }
                }
                Mode::VBlank => {
                    if self.cycles > 456.into() {
                        self.cycles %= 456;
                        self.pos.line_y += 1;

                        // New Scanline is next, check for LYC=LY
                        if self.stat.coincidence_int() {
                            let are_equal = self.pos.line_y == self.pos.ly_compare;
                            self.stat.set_coincidence(are_equal);
                        }

                        if self.pos.line_y == 154 {
                            self.pos.line_y = 0;

                            if self.stat.oam_int() {
                                // Enable OAM LCDStat Interrupt
                                self.int.set_lcd_stat(true);
                            }

                            self.stat.set_mode(Mode::OamScan);
                        }
                    }
                }
            }
        }
    }

    fn scan_oam(&mut self, cycle: u32) {
        if cycle % 2 != 0 {
            // This is run 50% of the time, or 40 times
            // which is the number of sprites in OAM

            let sprite_height = match self.control.obj_size() {
                ObjectSize::EightByEight => 8,
                ObjectSize::EightBySixteen => 16,
            };

            let attr = self.oam.attribute((cycle / 2) as usize);
            let line_y = self.pos.line_y + 16;

            if attr.x > 0 && line_y >= attr.y && line_y < (attr.y + sprite_height) {
                if !self.sprite_buffer.full() {
                    self.sprite_buffer.add(attr);
                }
            }
        }
    }

    fn draw(&mut self, cycle: u32) {
        use FetcherState::*;

        // By only running on odd cycles, we can ensure that we draw every two T cycles
        if cycle % 2 != 0 {
            let line_y = self.pos.line_y;
            let scroll_y = self.pos.scroll_y;
            let window_y = self.pos.window_y;
            let window_present = self.control.window_enabled() && window_y <= line_y;

            match self.fetcher.state {
                TileNumber => {
                    let scroll_x = self.pos.scroll_x;

                    // Increment Window line counter if scanline had any window pixels on it
                    // only increment once per scanline though
                    if window_present && !self.fetcher.window_line.already_checked() {
                        self.fetcher.window_line.increment();
                    }

                    // Determine which tile map is being used
                    let tile_map = if window_present {
                        self.control.win_tile_map_addr()
                    } else {
                        self.control.bg_tile_map_addr()
                    };
                    let tile_map_addr = tile_map.into_address();

                    // Both Offsets are used to offset the tile map address we found above
                    // Offsets are ANDed wih 0x3FF so that we stay in bounds of tile map memory
                    // TODO: Is this necessary / important in other fetcher modes?
                    let x_offset = (self.fetcher.x_pos + scroll_x) as u16 & 0x03FF;
                    let y_offset = (line_y.wrapping_add(scroll_y)) as u16 & 0x03FF;

                    // Scroll X Offset is only used when we're rendering the background;
                    let scx_offset = if window_present { 0 } else { scroll_x / 8 } & 0x1F;

                    let offset = if window_present {
                        32 * (self.fetcher.window_line.value() as u16 / 8)
                    } else {
                        32 * (((y_offset) & 0x00FF) / 8)
                    };

                    let addr = tile_map_addr + offset + x_offset + scx_offset as u16;

                    let id = self.read_byte(addr);
                    self.fetcher.builder.with_id(id);

                    // Move on to the Next state in 2 T-cycles
                    self.fetcher.state = TileDataLow;
                }
                TileDataLow => {
                    let id = self
                        .fetcher
                        .builder
                        .id
                        .expect("Tile Number unexpectedly missing");

                    let tile_data_addr = match self.control.tile_data_addr() {
                        TileDataAddress::X8800 => (0x9000_i32 + (id as i32 * 16)) as u16,
                        TileDataAddress::X8000 => 0x8000 + (id as u16 * 16),
                    };

                    let offset = if window_present {
                        2 * (self.fetcher.window_line.value() % 8)
                    } else {
                        2 * ((line_y + scroll_y) % 8)
                    };

                    let addr = tile_data_addr + offset as u16;
                    let low = self.read_byte(addr);
                    self.fetcher.builder.with_data_low(low);

                    self.fetcher.state = TileDataHigh;
                }
                TileDataHigh => {
                    let id = self
                        .fetcher
                        .builder
                        .id
                        .expect("Tile Number unexpectedly missing");

                    let tile_data_addr = match self.control.tile_data_addr() {
                        TileDataAddress::X8800 => (0x9000_i32 + (id as i32 * 16)) as u16,
                        TileDataAddress::X8000 => 0x8000 + (id as u16 * 16),
                    };

                    let offset = if window_present {
                        2 * (self.fetcher.window_line.value() % 8)
                    } else {
                        2 * ((line_y + scroll_y) % 8)
                    };

                    let addr = tile_data_addr + offset as u16;
                    let high = self.read_byte(addr + 1);
                    self.fetcher.builder.with_data_high(high);

                    self.fetcher.state = SendToFifo;
                }
                SendToFifo => {
                    if let Some(low) = self.fetcher.builder.low {
                        if let Some(high) = self.fetcher.builder.high {
                            let pixel = Pixels::from_bytes(high, low);
                            let palette = self.monochrome.bg_palette;

                            if self.fifo.background.is_empty() {
                                for i in 0..8 {
                                    // Horizontally flip pixels
                                    let bit = 7 - i;

                                    let shade = palette.colour(pixel.pixel(bit));

                                    let fifo_pixel = FifoPixel {
                                        kind: FifoPixelKind::Background,
                                        shade,
                                        palette: None,
                                        priority: None,
                                    };

                                    self.fifo.background.push_back(fifo_pixel);
                                }
                            }

                            self.fetcher.state = TileNumber;
                            self.fetcher.x_pos += 1;
                        }
                    }
                }
            }
        }

        // Handle Pixel and Sprite FIFO
        if let Some(bg_pixel) = self.fifo.background.pop_front() {
            if let Some(_sprite_pixel) = self.fifo.sprite.pop_front() {
                todo!("Mix the pixels or whatever I'm supposed todo here");
            } else {
                // Only Background Pixels will be rendered

                let y = self.pos.line_y as usize;
                let x = self.x_pos as usize;
                let rgba = bg_pixel.shade.into_rgba();

                let i = (GB_WIDTH * 4) * y + (x * 4);
                self.frame_buf[i..(i + rgba.len())].copy_from_slice(&rgba);
            }

            self.x_pos += 1;
        }
    }

    pub fn copy_to_gui(&self, frame: &mut [u8]) {
        frame.copy_from_slice(self.frame_buf.as_ref());
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            vram: Box::new([0u8; VRAM_SIZE]),
            cycles: 0.into(),
            frame_buf: Box::new([0; GB_WIDTH * GB_HEIGHT * 4]),
            int: Default::default(),
            control: Default::default(),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: Default::default(),
            oam: Default::default(),
            fetcher: Default::default(),
            fifo: Default::default(),
            sprite_buffer: Default::default(),
            x_pos: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Interrupt {
    _vblank: bool,
    _lcd_stat: bool,
}

impl Interrupt {
    pub fn vblank(&self) -> bool {
        self._vblank
    }

    pub fn set_vblank(&mut self, enabled: bool) {
        self._vblank = enabled;
    }

    pub fn lcd_stat(&self) -> bool {
        self._lcd_stat
    }

    pub fn set_lcd_stat(&mut self, enabled: bool) {
        self._lcd_stat = enabled;
    }
}

bitfield! {
    pub struct LCDStatus(u8);
    impl Debug;
    pub coincidence_int, set_coincidence_int: 6;
    pub oam_int, set_oam_int: 5;
    pub vblank_int, set_vblank_int: 4;
    pub hblank_int, set_hblank_int: 3;
    pub coincidence, set_coincidence: 2; // LYC == LY Flag
    from into Mode, _mode, set_mode: 1, 0;
}

impl LCDStatus {
    pub fn mode(&self) -> Mode {
        self._mode()
    }
}

impl Copy for LCDStatus {}
impl Clone for LCDStatus {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for LCDStatus {
    fn default() -> Self {
        Self(0x80) // bit 7 is always 1
    }
}

impl From<u8> for LCDStatus {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<LCDStatus> for u8 {
    fn from(status: LCDStatus) -> Self {
        status.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    HBlank = 0,
    VBlank = 1,
    OamScan = 2,
    Drawing = 3,
}

impl From<u8> for Mode {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::HBlank,
            0b01 => Self::VBlank,
            0b10 => Self::OamScan,
            0b11 => Self::Drawing,
            _ => unreachable!("{:#04X} is not a valid value for LCDMode", byte),
        }
    }
}

impl From<Mode> for u8 {
    fn from(mode: Mode) -> Self {
        mode as Self
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::HBlank
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ScreenPosition {
    pub scroll_y: u8,
    pub scroll_x: u8,
    pub line_y: u8,
    pub ly_compare: u8,
    pub window_y: u8,
    pub window_x: u8,
}

bitfield! {
    pub struct LCDControl(u8);
    impl Debug;
    lcd_enabled, set_lcd_enabled: 7;
    from into TileMapAddress, win_tile_map_addr, set_win_tile_map_addr: 6, 6;
    window_enabled, set_window_enabled: 5;
    from into TileDataAddress, tile_data_addr, set_tile_data_addr: 4, 4;
    from into TileMapAddress, bg_tile_map_addr, set_bg_tile_map_addr: 3, 3;
    from into ObjectSize, obj_size, set_obj_size: 2, 2;
    obj_enabled, set_obj_enabled: 1;
    bg_win_enabled, set_bg_win_enabled: 0;
}

impl Copy for LCDControl {}
impl Clone for LCDControl {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for LCDControl {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for LCDControl {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<LCDControl> for u8 {
    fn from(ctrl: LCDControl) -> Self {
        ctrl.0
    }
}

#[derive(Debug, Clone, Copy)]
enum TileMapAddress {
    X9800 = 0,
    X9C00 = 1,
}

impl TileMapAddress {
    pub fn into_address(self) -> u16 {
        match self {
            TileMapAddress::X9800 => 0x9800,
            TileMapAddress::X9C00 => 0x9C00,
        }
    }
}

impl From<u8> for TileMapAddress {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::X9800,
            0b01 => Self::X9C00,
            _ => unreachable!("{:#04X} is not a valid value for TileMapRegister", byte),
        }
    }
}

impl From<TileMapAddress> for u8 {
    fn from(reg: TileMapAddress) -> Self {
        reg as Self
    }
}

impl Default for TileMapAddress {
    fn default() -> Self {
        Self::X9800
    }
}

#[derive(Debug, Clone, Copy)]
enum TileDataAddress {
    X8800 = 0,
    X8000 = 1,
}

impl From<u8> for TileDataAddress {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::X8800,
            0b01 => Self::X8000,
            _ => unreachable!("{:#04X} is not a valid value for TileDataRegister", byte),
        }
    }
}

impl From<TileDataAddress> for u8 {
    fn from(reg: TileDataAddress) -> Self {
        reg as Self
    }
}

impl Default for TileDataAddress {
    fn default() -> Self {
        Self::X8800
    }
}

#[derive(Debug, Clone, Copy)]
enum ObjectSize {
    EightByEight = 0,
    EightBySixteen = 1,
}

impl From<u8> for ObjectSize {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::EightByEight,
            0b01 => Self::EightBySixteen,
            _ => unreachable!("{:#04X} is not a valid value for ObjSize", byte),
        }
    }
}

impl From<ObjectSize> for u8 {
    fn from(size: ObjectSize) -> Self {
        size as Self
    }
}

impl Default for ObjectSize {
    fn default() -> Self {
        Self::EightByEight
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GrayShade {
    White = 0,
    LightGray = 1,
    DarkGray = 2,
    Black = 3,
}

impl GrayShade {
    pub fn into_rgba(self) -> [u8; 4] {
        match self {
            GrayShade::White => WHITE,
            GrayShade::LightGray => LIGHT_GRAY,
            GrayShade::DarkGray => DARK_GRAY,
            GrayShade::Black => BLACK,
        }
    }

    pub fn from_rgba(slice: &[u8]) -> Self {
        let rgba: [u8; 4] = slice
            .try_into()
            .expect("Unable to interpret &[u8] as [u8; 4]");

        match rgba {
            WHITE => GrayShade::White,
            LIGHT_GRAY => GrayShade::LightGray,
            DARK_GRAY => GrayShade::DarkGray,
            BLACK => GrayShade::Black,
            _ => panic!("{:#04X?} is not a colour the DMG-01 supports", rgba),
        }
    }
}

impl Default for GrayShade {
    fn default() -> Self {
        Self::White
    }
}

impl From<u8> for GrayShade {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => GrayShade::White,
            0b01 => GrayShade::LightGray,
            0b10 => GrayShade::DarkGray,
            0b11 => GrayShade::Black,
            _ => unreachable!("{:#04X} is not a valid value for GrayShade", byte),
        }
    }
}

impl From<GrayShade> for u8 {
    fn from(shade: GrayShade) -> Self {
        shade as Self
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Monochrome {
    pub bg_palette: BackgroundPalette,
    pub obj_palette_0: ObjectPalette,
    pub obj_palette_1: ObjectPalette,
}

bitfield! {
    pub struct BackgroundPalette(u8);
    impl Debug;
    pub from into GrayShade, i3_colour, set_i3_colour: 7, 6;
    pub from into GrayShade, i2_colour, set_i2_colour: 5, 4;
    pub from into GrayShade, i1_colour, set_i1_colour: 3, 2;
    pub from into GrayShade, i0_colour, set_i0_colour: 1, 0;
}

impl BackgroundPalette {
    pub fn colour(&self, id: u8) -> GrayShade {
        match id {
            0b00 => self.i0_colour(),
            0b01 => self.i1_colour(),
            0b10 => self.i2_colour(),
            0b11 => self.i3_colour(),
            _ => unreachable!("{:#04X} is not a valid BG colour id", id),
        }
    }
}

impl Copy for BackgroundPalette {}
impl Clone for BackgroundPalette {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for BackgroundPalette {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for BackgroundPalette {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<BackgroundPalette> for u8 {
    fn from(palette: BackgroundPalette) -> Self {
        palette.0
    }
}

bitfield! {
    pub struct ObjectPalette(u8);
    impl Debug;
    pub from into GrayShade, i3_colour, set_i3_colour: 7, 6;
    pub from into GrayShade, i2_colour, set_i2_colour: 5, 4;
    pub from into GrayShade, i1_colour, set_i1_colour: 3, 2;
}

impl ObjectPalette {
    pub fn colour(&self, id: u8) -> Option<GrayShade> {
        match id {
            0b00 => None,
            0b01 => Some(self.i1_colour()),
            0b10 => Some(self.i2_colour()),
            0b11 => Some(self.i3_colour()),
            _ => unreachable!("{:#04X} is not a valid OBJ colour id", id),
        }
    }
}

impl Copy for ObjectPalette {}
impl Clone for ObjectPalette {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for ObjectPalette {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for ObjectPalette {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<ObjectPalette> for u8 {
    fn from(palette: ObjectPalette) -> Self {
        palette.0
    }
}

struct Pixels(u8, u8);

impl Pixels {
    pub fn from_bytes(higher: u8, lower: u8) -> Self {
        Self(higher, lower)
    }

    pub fn pixel(&self, bit: usize) -> u8 {
        let higher = self.0 >> bit;
        let lower = self.1 >> bit;

        (higher & 0x01) << 1 | lower & 0x01
    }
}

#[derive(Debug, Clone)]
pub struct SpriteAttributeTable {
    buf: Box<[u8; OAM_SIZE]>,
}

impl SpriteAttributeTable {
    pub fn read_byte(&self, addr: u16) -> u8 {
        let index = (addr - 0xFE00) as usize;
        self.buf[index]
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        let index = (addr - 0xFE00) as usize;
        self.buf[index] = byte;
    }

    pub fn attribute(&self, index: usize) -> SpriteAttribute {
        let slice: &[u8; 4] = self.buf[index..(index + 4)]
            .try_into()
            .expect("Could not interpret &[u8] as a &[u8; 4]");

        slice.into()
    }
}

impl Default for SpriteAttributeTable {
    fn default() -> Self {
        Self {
            buf: Box::new([0; OAM_SIZE]),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SpriteAttribute {
    y: u8,
    x: u8,
    tile_index: u8,
    flags: SpriteFlag,
}

impl From<[u8; 4]> for SpriteAttribute {
    fn from(bytes: [u8; 4]) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_index: bytes[2],
            flags: bytes[3].into(),
        }
    }
}

impl<'a> From<&'a [u8; 4]> for SpriteAttribute {
    fn from(bytes: &'a [u8; 4]) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_index: bytes[2],
            flags: bytes[3].into(),
        }
    }
}

bitfield! {
    pub struct SpriteFlag(u8);
    impl Debug;

    from into RenderPriority, priority, set_priority: 7, 7;
    y_flip, set_y_flip: 6;
    x_flip, set_x_flip: 5;
    from into SpritePaletteNumber, palette, set_palette: 4, 4;
}

impl Copy for SpriteFlag {}
impl Clone for SpriteFlag {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<u8> for SpriteFlag {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<SpriteFlag> for u8 {
    fn from(flags: SpriteFlag) -> Self {
        flags.0
    }
}

impl Default for SpriteFlag {
    fn default() -> Self {
        Self(0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RenderPriority {
    Sprite = 0,
    BackgroundAndWindow = 1,
}

impl From<u8> for RenderPriority {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Sprite,
            0b01 => Self::BackgroundAndWindow,
            _ => unreachable!("{:#04X} is not a valid value for RenderPriority", byte),
        }
    }
}

impl From<RenderPriority> for u8 {
    fn from(priority: RenderPriority) -> Self {
        priority as u8
    }
}

impl Default for RenderPriority {
    fn default() -> Self {
        Self::Sprite
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SpritePaletteNumber {
    SpritePalette0 = 0,
    SpritePalette1 = 1,
}

impl From<u8> for SpritePaletteNumber {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => SpritePaletteNumber::SpritePalette0,
            0b01 => SpritePaletteNumber::SpritePalette1,
            _ => unreachable!("{:#04X} is not a valid value for BgPaletteNumber", byte),
        }
    }
}

impl From<SpritePaletteNumber> for u8 {
    fn from(flip: SpritePaletteNumber) -> Self {
        flip as u8
    }
}

#[derive(Debug, Clone, Copy)]
struct SpriteBuffer {
    buf: [SpriteAttribute; 10],
    len: usize,
}

impl SpriteBuffer {
    pub fn full(&self) -> bool {
        self.len == self.buf.len()
    }

    pub fn _clear(&mut self) {
        self.buf = [Default::default(); 10];
        self.len = 0;
    }

    pub fn add(&mut self, attr: SpriteAttribute) {
        self.buf[self.len] = attr;
        self.len += 1;
    }
}

impl Default for SpriteBuffer {
    fn default() -> Self {
        Self {
            buf: [Default::default(); SPRITE_BUFFER_LIMIT],
            len: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct PixelFetcher {
    state: FetcherState,
    x_pos: u8,
    window_line: WindowLineCounter,
    builder: TileBuilder,
}

impl PixelFetcher {
    pub fn hblank_reset(&mut self) {
        self.window_line.hblank_reset();

        self.builder = Default::default();
        self.state = Default::default();
        self.x_pos = 0;
    }

    pub fn vblank_reset(&mut self) {
        self.window_line.vblank_reset();
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct WindowLineCounter {
    value: u8,
    already_checked: bool,
}

impl WindowLineCounter {
    pub fn already_checked(&self) -> bool {
        self.already_checked
    }

    pub fn increment(&mut self) {
        self.value += 1;
        self.already_checked = true;
    }

    pub fn hblank_reset(&mut self) {
        self.already_checked = false;
    }

    pub fn vblank_reset(&mut self) {
        self.value = 0;
        self.already_checked = false;
    }

    pub fn value(&self) -> u8 {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FetcherState {
    TileNumber,
    TileDataLow,
    TileDataHigh,
    SendToFifo,
}

impl Default for FetcherState {
    fn default() -> Self {
        Self::TileNumber
    }
}

#[derive(Debug, Clone, Copy)]
enum FifoPixelKind {
    Background,
    Sprite,
}

impl Default for FifoPixelKind {
    fn default() -> Self {
        Self::Background
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct FifoPixel {
    kind: FifoPixelKind,
    shade: GrayShade,
    palette: Option<ObjectPalette>,
    priority: Option<RenderPriority>,
}

// FIXME: Fifo Registers have a known size. Are heap allocations
// really necessary here?
#[derive(Debug, Clone)]
struct FifoRenderer {
    background: VecDeque<FifoPixel>,
    sprite: VecDeque<FifoPixel>,
}

impl Default for FifoRenderer {
    fn default() -> Self {
        Self {
            background: VecDeque::with_capacity(8),
            sprite: VecDeque::with_capacity(8),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct TileBuilder {
    id: Option<u8>,
    low: Option<u8>,
    high: Option<u8>,
}

impl TileBuilder {
    pub fn with_id(&mut self, id: u8) {
        self.id = Some(id);
    }

    pub fn with_data_low(&mut self, data: u8) {
        self.low = Some(data);
    }

    pub fn with_data_high(&mut self, data: u8) {
        self.high = Some(data);
    }
}
