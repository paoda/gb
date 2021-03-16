use crate::instruction::Cycles;

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

#[derive(Debug, Clone, Copy, Default)]
pub struct LCDStatus {
    lyc_eq_ly: bool,
    mode2_stat: bool,
    mode1_stat: bool,
    mode0_stat: bool,
    coincidence: bool,
    ppu_mode: u8,
}

impl From<u8> for LCDStatus {
    fn from(byte: u8) -> Self {
        Self {
            lyc_eq_ly: (byte >> 6) & 0x01 == 0x01,
            mode2_stat: (byte >> 5) & 0x01 == 0x01,
            mode1_stat: (byte >> 4) & 0x01 == 0x01,
            mode0_stat: (byte >> 3) & 0x01 == 0x01,
            coincidence: (byte >> 2) & 0x01 == 0x01,
            ppu_mode: byte & 0x03,
        }
    }
}

impl From<LCDStatus> for u8 {
    fn from(status: LCDStatus) -> Self {
        0x80 | (status.lyc_eq_ly as u8) << 6
            | (status.mode2_stat as u8) << 5
            | (status.mode1_stat as u8) << 4
            | (status.mode0_stat as u8) << 3
            | (status.coincidence as u8) << 2
            | (status.ppu_mode & 0x03)
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

#[derive(Debug, Default, Clone, Copy)]
pub struct LCDControl {
    lcd_enable: bool, // Bit 7
    window_tile_map_select: bool,
    window_enable: bool,
    tile_data_select: bool,
    bg_tile_map_select: bool,
    sprite_size: bool,
    sprite_enable: bool,
    display_priority: bool, // Bit 0
}

impl From<u8> for LCDControl {
    fn from(byte: u8) -> Self {
        Self {
            lcd_enable: (byte >> 7) == 0x01,
            window_tile_map_select: ((byte >> 6) & 0x01) == 0x01,
            window_enable: ((byte >> 5) & 0x01) == 0x01,
            tile_data_select: ((byte >> 4) & 0x01) == 0x01,
            bg_tile_map_select: ((byte >> 3) & 0x01) == 0x01,
            sprite_size: ((byte >> 2) & 0x01) == 0x01,
            sprite_enable: ((byte >> 1) & 0x01) == 0x01,
            display_priority: (byte & 0x01) == 0x01,
        }
    }
}

impl From<LCDControl> for u8 {
    fn from(ctrl: LCDControl) -> Self {
        (ctrl.lcd_enable as u8) << 7
            | (ctrl.window_tile_map_select as u8) << 6
            | (ctrl.window_enable as u8) << 5
            | (ctrl.tile_data_select as u8) << 4
            | (ctrl.bg_tile_map_select as u8) << 3
            | (ctrl.sprite_size as u8) << 2
            | (ctrl.sprite_enable as u8) << 1
            | ctrl.display_priority as u8
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
