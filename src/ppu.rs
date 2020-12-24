#[derive(Debug, Clone)]
pub struct PPU {
    lcdc: LCDControl,
    pub vram: [u8; 8192],
}

impl Default for PPU {
    fn default() -> Self {
        Self {
            lcdc: Default::default(),
            vram: [0; 8192],
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct LCDControl {
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
