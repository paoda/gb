use crate::instruction::Cycles;
use bitfield::bitfield;

const GB_WIDTH: usize = 160;
const GB_HEIGHT: usize = 144;
#[derive(Debug, Clone)]
pub struct Ppu {
    pub lcd_control: LCDControl,
    pub monochrome: Monochrome,
    pub pos: ScreenPosition,
    pub vram: Box<[u8; 8192]>,
    pub oam: Box<[u8; 160]>,
    frame_buf: [u8; GB_WIDTH * GB_HEIGHT * 4],
    pub stat: LCDStatus,
    cycles: Cycles,
}

impl Ppu {
    pub fn read_byte(&self, addr: u16) -> u8 {
        unimplemented!()
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        unimplemented!()
    }
}

impl Ppu {
    pub fn step(&mut self, cycles: Cycles) {
        self.cycles += cycles;

        // let tmp: u32 = self.cycles.into();
        // println!("Mode: {:?} | Cycles: {}", self.mode, tmp);

        match self.stat.mode() {
            Mode::OamScan => {
                if self.cycles >= 80.into() {
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
                    self.stat.set_mode(Mode::HBlank);
                }
            }
            Mode::HBlank => {
                // We've reached the end of a scanline
                if self.cycles >= 456.into() {
                    self.pos.line_y += 1;

                    let next_mode = if self.pos.line_y >= 143 {
                        Mode::VBlank
                    } else {
                        Mode::OamScan
                    };

                    self.stat.set_mode(next_mode);
                }
            }
            Mode::VBlank => {
                // We've reached the end of the screen

                if self.cycles >= 456.into() {
                    self.cycles %= 456;
                    self.pos.line_y += 1;

                    if self.pos.line_y >= 153 {
                        self.stat.set_mode(Mode::OamScan);
                        self.pos.line_y = 0;
                    }
                }
            }
        }
    }

    pub fn draw(&self, frame: &mut [u8]) {
        frame.copy_from_slice(&self.frame_buf);
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            lcd_control: Default::default(),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: Default::default(),
            vram: Box::new([0u8; 8192]),
            oam: Box::new([0u8; 160]),
            cycles: 0.into(),
            frame_buf: [0; GB_WIDTH * GB_HEIGHT * 4],
        }
    }
}
bitfield! {
    pub struct LCDStatus(u8);
    impl Debug;
    pub lyc_ly_intr, set_lyc_ly_intr: 6;
    pub oam_intr, set_oam_intr: 5;
    pub vblank_intr, set_vblank_intr: 4;
    pub hblank_intr, set_hblank_intr: 3;
    pub coincidence, _: 2; // LYC == LY Flag
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
        Self(0)
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
        mode as u8
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
    pub ly_compare: bool,
    pub window_y: u8,
    pub window_x: u8,
}

bitfield! {
    pub struct LCDControl(u8);
    impl Debug;
    lcd_enabled, set_lcd_enabled: 7;
    from into TileMapRegister, win_tile_map_area, set_win_tile_map_area: 6;
    window_enabled, set_window_enabled: 5;
    from into TileDataRegister, tile_data_area, set_tile_data_area: 4;
    from into TileMapRegister, gb_tile_map_area, set_gb_tile_map_area: 3;
    from into OBJSize, obg_size, set_obj_size: 2;
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
enum TileMapRegister {
    X9800 = 0,
    X9C00 = 1,
}

impl From<u8> for TileMapRegister {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::X9800,
            0b01 => Self::X9C00,
            _ => unreachable!("{:#04X} is not a valid value for TileMapRegister", byte),
        }
    }
}

impl Default for TileMapRegister {
    fn default() -> Self {
        Self::X9800
    }
}

#[derive(Debug, Clone, Copy)]
enum TileDataRegister {
    X8800 = 0,
    X8000 = 1,
}

impl From<u8> for TileDataRegister {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::X8800,
            0b01 => Self::X8000,
            _ => unreachable!("{:#04X} is not a valid value for TileDataRegister", byte),
        }
    }
}

impl Default for TileDataRegister {
    fn default() -> Self {
        Self::X8800
    }
}

#[derive(Debug, Clone, Copy)]
enum ObjSize {
    EightByEight = 0,
    EightBySixteen = 1,
}

impl From<u8> for ObjSize {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::EightByEight,
            0b01 => Self::EightBySixteen,
            _ => unreachable!("{:#04X} is not a valid value for ObjSize", byte),
        }
    }
}

impl Default for ObjSize {
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
        shade as u8
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
