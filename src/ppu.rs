#[derive(Debug, Clone)]
pub struct PPU {
    pub lcd_control: LCDControl,
    pub monochrome: Monochrome,
    pub vram: Box<[u8]>,
}

impl Default for PPU {
    fn default() -> Self {
        Self {
            lcd_control: Default::default(),
            monochrome: Default::default(),
            vram: vec![0; 8192].into_boxed_slice(),
        }
    }
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
    fn from(lcdc: LCDControl) -> Self {
        (lcdc.lcd_enable as u8) << 7
            | (lcdc.window_tile_map_select as u8) << 6
            | (lcdc.window_enable as u8) << 5
            | (lcdc.tile_data_select as u8) << 4
            | (lcdc.bg_tile_map_select as u8) << 3
            | (lcdc.sprite_size as u8) << 2
            | (lcdc.sprite_enable as u8) << 1
            | lcdc.display_priority as u8
    }
}

#[derive(Debug, Clone, Copy)]
enum GrayShade {
    White,
    LightGray,
    DarkGray,
    Black,
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
            _ => unreachable!("{:#04X} is not a valid Shade of Gray", byte),
        }
    }
}

impl From<GrayShade> for u8 {
    fn from(shade: GrayShade) -> Self {
        match shade {
            GrayShade::White => 0b00,
            GrayShade::LightGray => 0b01,
            GrayShade::DarkGray => 0b10,
            GrayShade::Black => 0b11,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BackgroundPalette {
    color3: GrayShade,
    color2: GrayShade,
    color1: GrayShade,
    color0: GrayShade,
}

impl From<u8> for BackgroundPalette {
    fn from(byte: u8) -> Self {
        Self {
            color3: (byte >> 6).into(),
            color2: ((byte >> 4) & 0x03).into(),
            color1: ((byte >> 2) & 0x03).into(),
            color0: (byte & 0x03).into(),
        }
    }
}

impl From<BackgroundPalette> for u8 {
    fn from(palette: BackgroundPalette) -> Self {
        let color0: u8 = palette.color0.into();
        let color1: u8 = palette.color1.into();
        let color2: u8 = palette.color2.into();
        let color3: u8 = palette.color0.into();

        color3 << 6 | color2 << 4 | color1 << 2 | color0
    }
}
