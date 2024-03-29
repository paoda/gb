use super::{BLACK, DARK_GRAY, LIGHT_GRAY, WHITE};
use bitfield::bitfield;

bitfield! {
    pub struct LCDStatus(u8);
    impl Debug;
    pub coincidence_int, set_coincidence_int: 6;
    pub oam_int, set_oam_int: 5;
    pub vblank_int, set_vblank_int: 4;
    pub hblank_int, set_hblank_int: 3;
    pub coincidence, set_coincidence: 2; // LYC == LY Flag
    pub from into PpuMode, mode, set_mode: 1, 0;
}

impl LCDStatus {
    pub(crate) fn update(&mut self, byte: u8) {
        // Bytes 2 -> 0 are read only
        let mask = 0b00000111;

        let read_only = self.0 & mask;
        self.0 = (byte & !mask) | read_only;
    }
}

impl Copy for LCDStatus {}
impl Clone for LCDStatus {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<LCDStatus> for u8 {
    fn from(status: LCDStatus) -> Self {
        status.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PpuMode {
    HBlank = 0,
    VBlank = 1,
    OamScan = 2,
    Drawing = 3,
}

impl From<u8> for PpuMode {
    fn from(byte: u8) -> Self {
        match byte & 0b11 {
            0b00 => Self::HBlank,
            0b01 => Self::VBlank,
            0b10 => Self::OamScan,
            0b11 => Self::Drawing,
            _ => unreachable!("{:#04X} is not a valid value for LCDMode", byte),
        }
    }
}

impl From<PpuMode> for u8 {
    fn from(mode: PpuMode) -> Self {
        mode as Self
    }
}

bitfield! {
    pub struct LCDControl(u8);
    impl Debug;
    pub lcd_enabled, set_lcd_enabled: 7;
    pub from into TileMapAddress, win_tile_map_addr, set_win_tile_map_addr: 6, 6;
    pub window_enabled, set_window_enabled: 5;
    pub from into TileDataAddress, tile_data_addr, set_tile_data_addr: 4, 4;
    pub from into TileMapAddress, bg_tile_map_addr, set_bg_tile_map_addr: 3, 3;
    pub from into ObjectSize, obj_size, set_obj_size: 2, 2;
    pub obj_enabled, set_obj_enabled: 1;
    pub bg_win_enabled, set_bg_win_enabled: 0;
}

impl Copy for LCDControl {}
impl Clone for LCDControl {
    fn clone(&self) -> Self {
        *self
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
pub enum TileMapAddress {
    X9800 = 0,
    X9C00 = 1,
}

impl TileMapAddress {
    pub(crate) fn into_address(self) -> u16 {
        match self {
            TileMapAddress::X9800 => 0x9800,
            TileMapAddress::X9C00 => 0x9C00,
        }
    }
}

impl From<u8> for TileMapAddress {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
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

#[derive(Debug, Clone, Copy)]
pub enum TileDataAddress {
    X8800 = 0,
    X8000 = 1,
}

impl From<u8> for TileDataAddress {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
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

#[derive(Debug, Clone, Copy)]
pub enum ObjectSize {
    Eight = 0,
    Sixteen = 1,
}

impl ObjectSize {
    pub(crate) fn size(&self) -> u8 {
        use ObjectSize::*;

        match self {
            Eight => 8,
            Sixteen => 16,
        }
    }
}

impl From<u8> for ObjectSize {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Eight,
            0b01 => Self::Sixteen,
            _ => unreachable!("{:#04X} is not a valid value for ObjSize", byte),
        }
    }
}

impl From<ObjectSize> for u8 {
    fn from(size: ObjectSize) -> Self {
        size as Self
    }
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
    pub(crate) fn shade(&self, id: u8) -> GrayShade {
        match id & 0b11 {
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
    pub(crate) fn shade(&self, id: u8) -> Option<GrayShade> {
        match id & 0b11 {
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

pub(crate) struct Pixels(u8, u8);

impl Pixels {
    pub(crate) const PIXEL_COUNT: usize = 8;

    pub(crate) fn from_bytes(higher: u8, lower: u8) -> Self {
        Self(higher, lower)
    }

    pub(crate) fn shade_id(&self, x: usize) -> u8 {
        let bit = 7 - x;

        let higher = self.0 >> bit;
        let lower = self.1 >> bit;

        (higher & 0x01) << 1 | lower & 0x01
    }
}

bitfield! {
    pub struct ObjectFlags(u8);
    impl Debug;

    pub from into RenderPriority, priority, set_priority: 7, 7;
    pub y_flip, set_y_flip: 6;
    pub x_flip, set_x_flip: 5;
    pub from into ObjectPaletteKind, palette, set_palette: 4, 4;
}

impl Eq for ObjectFlags {}
impl PartialEq for ObjectFlags {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Copy for ObjectFlags {}
impl Clone for ObjectFlags {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<u8> for ObjectFlags {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<ObjectFlags> for u8 {
    fn from(flags: ObjectFlags) -> Self {
        flags.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ObjectPaletteKind {
    Zero = 0,
    One = 1,
}

impl From<u8> for ObjectPaletteKind {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => ObjectPaletteKind::Zero,
            0b01 => ObjectPaletteKind::One,
            _ => unreachable!("{:#04X} is not a valid value for BgPaletteNumber", byte),
        }
    }
}

impl From<ObjectPaletteKind> for u8 {
    fn from(palette_num: ObjectPaletteKind) -> Self {
        palette_num as u8
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RenderPriority {
    Object = 0,
    BackgroundAndWindow = 1,
}

impl From<u8> for RenderPriority {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Object,
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

#[derive(Debug, Clone, Copy)]
pub enum GrayShade {
    White = 0,
    LightGray = 1,
    DarkGray = 2,
    Black = 3,
}

impl GrayShade {
    pub(crate) fn into_rgba(self) -> [u8; 4] {
        match self {
            GrayShade::White => WHITE,
            GrayShade::LightGray => LIGHT_GRAY,
            GrayShade::DarkGray => DARK_GRAY,
            GrayShade::Black => BLACK,
        }
    }
}

impl From<u8> for GrayShade {
    fn from(byte: u8) -> Self {
        match byte & 0b11 {
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
