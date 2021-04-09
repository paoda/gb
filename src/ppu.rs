use std::convert::TryInto;

use crate::Cycle;
use crate::GB_HEIGHT;
use crate::GB_WIDTH;
use bitfield::bitfield;

const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;
const PPU_START_ADDRESS: usize = 0x8000;

const WHITE: [u8; 4] = [0xFF, 0xFF, 0xFF, 0xFF];
const LIGHT_GRAY: [u8; 4] = [0xCC, 0xCC, 0xCC, 0xFF];
const DARK_GRAY: [u8; 4] = [0x77, 0x77, 0x77, 0xFF];
const BLACK: [u8; 4] = [0x00, 0x00, 0x00, 0x00];

#[derive(Debug, Clone)]
pub struct Ppu {
    pub int: Interrupt,
    pub lcd_control: LCDControl,
    pub monochrome: Monochrome,
    pub pos: ScreenPosition,
    pub vram: Box<[u8; VRAM_SIZE]>,
    pub stat: LCDStatus,
    pub oam: SpriteAttributeTable,
    frame_buf: [u8; GB_WIDTH * GB_HEIGHT * 4],
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
        self.cycles += cycles;

        match self.stat.mode() {
            Mode::OamScan => {
                if self.cycles >= 80.into() {
                    self.cycles %= 80;
                    self.stat.set_mode(Mode::Drawing);
                }
            }
            Mode::Drawing => {
                // This mode can take from 172 -> 289 Cycles
                // Remember: There's no guarantee that we start this mode
                // with self.cycles == 80, since we aren't going for an accurate
                // emulator

                // TODO: This 172 needs to be variable somehow?
                if self.cycles >= 172.into() {
                    self.cycles %= 172;

                    if self.stat.hblank_int() {
                        self.int.set_lcd_stat(true);
                    }

                    self.stat.set_mode(Mode::HBlank);
                    self.draw_scanline();
                }
            }
            Mode::HBlank => {
                // We've reached the end of a scanline
                if self.cycles >= 200.into() {
                    self.cycles %= 200;
                    self.pos.line_y += 1;

                    let next_mode = if self.pos.line_y >= 144 {
                        self.int.set_vblank(true);

                        if self.stat.vblank_int() {
                            self.int.set_lcd_stat(true);
                        }

                        Mode::VBlank
                    } else {
                        if self.stat.oam_int() {
                            self.int.set_lcd_stat(true);
                        }

                        Mode::OamScan
                    };

                    self.stat.set_mode(next_mode);

                    if self.stat.coincidence_int() {
                        let are_equal = self.pos.line_y == self.pos.ly_compare;
                        self.stat.set_coincidence(are_equal);
                    }
                }
            }
            Mode::VBlank => {
                // We've reached the end of the screen

                if self.cycles >= 456.into() {
                    self.cycles %= 456;
                    self.pos.line_y += 1;

                    if self.pos.line_y == 154 {
                        self.pos.line_y = 0;

                        if self.stat.oam_int() {
                            self.int.set_lcd_stat(true);
                        }

                        self.stat.set_mode(Mode::OamScan);
                    }

                    if self.stat.coincidence_int() {
                        let are_equal = self.pos.line_y == self.pos.ly_compare;
                        self.stat.set_coincidence(are_equal);
                    }
                }
            }
        }
    }

    fn draw_scanline(&mut self) {
        let mut scanline: [u8; GB_WIDTH * 4] = [0; GB_WIDTH * 4];

        let window_x = self.pos.window_x.wrapping_sub(7);

        // True if a window is supposed to be drawn on this scanline
        let window_present =
            self.lcd_control.window_enabled() && self.pos.window_y <= self.pos.line_y;

        let tile_map = if window_present {
            self.lcd_control.win_tile_map_addr()
        } else {
            self.lcd_control.bg_tile_map_addr()
        };

        let tile_map_addr = tile_map.into_address();

        let pos_y = if window_present {
            self.pos.line_y.wrapping_sub(self.pos.window_y)
        } else {
            self.pos.line_y.wrapping_add(self.pos.scroll_y)
        };

        // There are always 20 rows of tiles in the LCD Viewport
        // 160 / 20 = 8, so we can figure out the row of a tile with the following
        let tile_row = pos_y / 8;

        for (i, chunk) in scanline.chunks_mut(4).enumerate() {
            let line_x = i as u8;
            let mut pos_x = line_x.wrapping_add(self.pos.scroll_x);

            if window_present {
                if line_x >= window_x {
                    pos_x = line_x.wrapping_sub(window_x);
                }
            }

            // There are always 18 columns of tiles in the LCD Viewport
            // 144 / 18 = 8, so we can figure out the column of a tile with the following
            let tile_column = pos_x / 8;

            // A tile is 8 x 8, and any given pixel in a tile comes from two bytes
            // so the size of a tile is (8 + 8) * 2 which is 32
            let tile_addr = tile_map_addr + (tile_row as u16) * 32 + tile_column as u16;
            let tile_number = self.read_byte(tile_addr);

            let tile_data_addr = match self.lcd_control.tile_data_addr() {
                TileDataAddress::X8800 => (0x9000_i32 + (tile_number as i32 * 16)) as u16,
                TileDataAddress::X8000 => 0x8000 + (tile_number as u16 * 16),
            };

            // Find the correct vertical line we're on
            let line = (pos_y % 8) * 2; // *2 since each vertical line takes up 2 bytes

            let higher = self.read_byte(tile_data_addr + line as u16);
            let lower = self.read_byte(tile_data_addr + line as u16 + 1);
            let pixels = Pixels::from_bytes(higher, lower);

            let bit = pos_x as usize % 8;
            let palette = self.monochrome.bg_palette;
            let shade = palette.colour(pixels.pixel(7 - bit)); // Flip Horizontally

            chunk.copy_from_slice(&shade.into_rgba());
        }

        let i = (GB_WIDTH * 4) * self.pos.line_y as usize;
        self.frame_buf[i..(i + scanline.len())].copy_from_slice(&scanline);
    }

    pub fn copy_to_gui(&self, frame: &mut [u8]) {
        frame.copy_from_slice(&self.frame_buf);
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            int: Interrupt::default(),
            lcd_control: Default::default(),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: Default::default(),
            vram: Box::new([0u8; VRAM_SIZE]),
            oam: Default::default(),
            cycles: 0.into(),
            frame_buf: [0; GB_WIDTH * GB_HEIGHT * 4],
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
    from into ObjectSize, obg_size, set_obj_size: 2, 2;
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
            _ => unreachable!("{:#04X} is not a valid colour id", id),
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

struct Pixels([u8; 2]);

impl Pixels {
    pub fn from_bytes(higher: u8, lower: u8) -> Self {
        Self([higher, lower])
    }

    pub fn pixel(&self, bit: usize) -> u8 {
        let higher = self.0[0] >> bit;
        let lower = self.0[1] >> bit;

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
}

impl SpriteAttributeTable {
    pub fn read_attribute(&self, addr: u16) -> SpriteAttribute {
        let buf_index = (addr - 0xFE00) as usize;
        self.attribute(buf_index)
    }

    pub fn attribute(&self, index: usize) -> SpriteAttribute {
        let bytes: [u8; 4] = self.buf[index..(index + 4)]
            .try_into()
            .expect("Byte slice was not four bytes in length");

        bytes.into()
    }
}

impl Default for SpriteAttributeTable {
    fn default() -> Self {
        Self {
            buf: Box::new([0; OAM_SIZE]),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpriteAttribute {
    x: u8,
    y: u8,
    tile_index: u8,
    attributes: SpriteFlag,
}

impl From<[u8; 4]> for SpriteAttribute {
    fn from(bytes: [u8; 4]) -> Self {
        Self {
            x: bytes[0],
            y: bytes[1],
            tile_index: bytes[2],
            attributes: bytes[3].into(),
        }
    }
}

bitfield! {
    pub struct SpriteFlag(u8);
    impl Debug;

    bg_window_over_obj, set_bg_window_over_obj: 7;
    from into SpriteFlip, y_flip, set_y_flip: 6, 6;
    from into SpriteFlip, x_flit, set_x_flip: 5, 5;
    from into BgPaletteNumber, palette, set_palette: 4, 4;
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
#[derive(Debug, Clone, Copy)]
pub enum SpriteFlip {
    Normal = 0,
    HorizontalMirror = 1,
}

impl From<u8> for SpriteFlip {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => SpriteFlip::Normal,
            0b01 => SpriteFlip::HorizontalMirror,
            _ => unreachable!("{:#04X} is not a valid value for SpriteFlip", byte),
        }
    }
}

impl From<SpriteFlip> for u8 {
    fn from(flip: SpriteFlip) -> Self {
        flip as u8
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BgPaletteNumber {
    BgPalette0 = 0,
    BgPalette1 = 1,
}

impl From<u8> for BgPaletteNumber {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => BgPaletteNumber::BgPalette0,
            0b01 => BgPaletteNumber::BgPalette1,
            _ => unreachable!("{:#04X} is not a valid value for BgPaletteNumber", byte),
        }
    }
}

impl From<BgPaletteNumber> for u8 {
    fn from(flip: BgPaletteNumber) -> Self {
        flip as u8
    }
}
