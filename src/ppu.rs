use crate::instruction::Cycles;
use bitfield::bitfield;

const GB_WIDTH: usize = 160;
const GB_HEIGHT: usize = 144;
#[derive(Debug, Clone)]
pub struct PPU {
    pub lcd_control: LCDControl,
    pub monochrome: Monochrome,
    pub pos: ScreenPosition,
    pub vram: Box<[u8]>,
    pub oam: Box<[u8]>,
    frame_buf: [u8; GB_WIDTH * GB_HEIGHT * 4],
    pub stat: LCDStatus,
    cycles: Cycles,
    mode: Mode,
}

impl PPU {
    pub fn step(&mut self, cycles: Cycles) {
        self.cycles += cycles;

        // let tmp: u32 = self.cycles.into();
        // println!("Mode: {:?} | Cycles: {}", self.mode, tmp);

        match self.mode {
            Mode::OAMScan => {
                if self.cycles >= 80.into() {
                    self.cycles %= 80;
                    self.mode = Mode::Draw;
                }
            }
            Mode::Draw => {
                if self.cycles >= 172.into() {
                    // 172 -> 129 Cycles
                    // self.cycles %= 172;
                    self.mode = Mode::HBlank;
                }
            }
            Mode::HBlank => {
                // The 80 comes from the 80 cycles we made disappear in OAMScan above.
                if self.cycles >= (456 - 80).into() {
                    self.cycles %= 456 - 80;
                    self.pos.line_y += 1;

                    if self.pos.line_y >= 144 {
                        self.mode = Mode::VBlank;
                    }
                }
            }
            Mode::VBlank => {
                if self.cycles >= 456.into() {
                    self.cycles %= 456;
                    self.pos.line_y += 1;

                    if self.pos.line_y == 154 {
                        self.mode = Mode::OAMScan;
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

impl Default for PPU {
    fn default() -> Self {
        Self {
            lcd_control: Default::default(),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: Default::default(),
            vram: vec![0; 8192].into_boxed_slice(),
            oam: vec![0; 160].into_boxed_slice(),
            cycles: 0.into(),
            frame_buf: [0; GB_WIDTH * GB_HEIGHT * 4],
            mode: Mode::OAMScan,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    OAMScan,
    Draw,
    HBlank,
    VBlank,
}

bitfield! {
    pub struct LCDStatus(u8);
    impl Debug;
    pub lyc_ly_intr, set_lyc_ly_intr: 6;
    pub oam_intr, set_oam_intr: 5;
    pub vblank_intr, set_vblank_intr: 4;
    pub hblank_intr, set_hblank_intr: 3;
    pub lyc_ly_flag, _: 2;
    pub from into LCDMode, mode, _: 1, 0;
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
pub enum LCDMode {
    HBlank = 0,
    VBlank = 1,
    OAM = 2,
    Transfer = 3,
}

impl From<u8> for LCDMode {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::HBlank,
            0b01 => Self::VBlank,
            0b10 => Self::OAM,
            0b11 => Self::Transfer,
            _ => unreachable!("{:#04X} is not a valid value for LCDMode", byte),
        }
    }
}

impl Default for LCDMode {
    fn default() -> Self {
        Self::HBlank
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ScreenPosition {
    pub scroll_y: u8,
    pub scroll_x: u8,
    pub line_y: u8,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Monochrome {
    pub bg_palette: BackgroundPalette,
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
enum GrayShade {
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

#[derive(Debug, Clone, Copy, Default)]
pub struct BackgroundPalette {
    colour: GrayShade,
    colour2: GrayShade,
    colour3: GrayShade,
    colour0: GrayShade, // FIXME: Is this supposed to be colour0?
}

impl From<u8> for BackgroundPalette {
    fn from(byte: u8) -> Self {
        Self {
            colour: (byte >> 6).into(),
            colour2: ((byte >> 4) & 0x03).into(),
            colour3: ((byte >> 2) & 0x03).into(),
            colour0: (byte & 0x03).into(),
        }
    }
}

impl From<BackgroundPalette> for u8 {
    fn from(palette: BackgroundPalette) -> Self {
        // FIXME: There is a bug here, see the above FIXME
        let colour0: u8 = palette.colour0 as u8;
        let colour1: u8 = palette.colour3 as u8;
        let colour2: u8 = palette.colour2 as u8;
        let colour3: u8 = palette.colour0 as u8;

        colour3 << 6 | colour2 << 4 | colour1 << 2 | colour0
    }
}
