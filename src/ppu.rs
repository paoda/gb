use crate::Cycle;
use crate::GB_HEIGHT;
use crate::GB_WIDTH;
use std::collections::VecDeque;
use std::convert::TryInto;

use self::types::{
    BackgroundPalette, GrayShade, LCDControl, LCDStatus, ObjectFlags, ObjectPalette,
    ObjectPaletteId, ObjectSize, Pixels, PpuMode, RenderPriority, TileDataAddress,
};

mod types;

const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;
const PPU_START_ADDRESS: usize = 0x8000;

// OAM Scan
const OBJECT_LIMIT: usize = 10;

// // White
// const WHITE: [u8; 4] = 0xFFFFFFFFu32.to_be_bytes();
// const LIGHT_GRAY: [u8; 4] = 0xB6B6B6FFu32.to_be_bytes();
// const DARK_GRAY: [u8; 4] = 0x676767FFu32.to_be_bytes();
// const BLACK: [u8; 4] = 0x000000FFu32.to_be_bytes();

// Green
const WHITE: [u8; 4] = 0xE3EEC0FFu32.to_be_bytes();
const LIGHT_GRAY: [u8; 4] = 0xAEBA89FFu32.to_be_bytes();
const DARK_GRAY: [u8; 4] = 0x5E6745FFu32.to_be_bytes();
const BLACK: [u8; 4] = 0x202020FFu32.to_be_bytes();

#[derive(Debug, Clone)]
pub struct Ppu {
    pub int: Interrupt,
    pub control: LCDControl,
    pub monochrome: Monochrome,
    pub pos: ScreenPosition,
    pub vram: Box<[u8; VRAM_SIZE]>,
    pub stat: LCDStatus,
    pub oam: ObjectAttributeTable,
    scan_state: OamScanState,
    fetch: PixelFetcher,
    fifo: FifoRenderer,
    obj_buffer: ObjectBuffer,
    frame_buf: Box<[u8; GB_WIDTH * GB_HEIGHT * 4]>,
    window_stat: WindowStatus,
    x_pos: u8,
    cycle: Cycle,
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
        let start: u32 = self.cycle.into();
        let end: u32 = cycles.into();

        for _ in start..(start + end) {
            self.cycle += 1;

            match self.stat.mode() {
                PpuMode::OamScan => {
                    if self.cycle >= 80.into() {
                        self.stat.set_mode(PpuMode::Drawing);
                    }

                    self.scan_oam();
                }
                PpuMode::Drawing => {
                    if self.x_pos >= 160 {
                        if self.stat.hblank_int() {
                            // Enable HBlank LCDStat Interrupt
                            self.int.set_lcd_stat(true);
                        }

                        // Done with rendering this frame,
                        // we can reset the ppu x_pos and fetcher state now

                        // Increment Window line counter if scanline had any window pixels on it
                        // only increment once per scanline though
                        if self.window_stat.should_draw() {
                            self.fetch.back.window_line.increment();
                        }

                        self.x_pos = 0;

                        self.fetch.hblank_reset();
                        self.window_stat.hblank_reset();
                        self.obj_buffer.clear();

                        self.fifo.back.clear();
                        self.fifo.obj.clear();

                        self.stat.set_mode(PpuMode::HBlank);
                    } else if self.control.lcd_enabled() {
                        // Only Draw when the LCD Is Enabled
                        self.draw(self.cycle.into());
                    } else {
                        self.reset();
                    }
                }
                PpuMode::HBlank => {
                    // This mode will always end at 456 cycles

                    if self.cycle >= 456.into() {
                        self.cycle %= 456;
                        self.pos.line_y += 1;

                        // Update LY==LYC bit
                        let are_equal = self.pos.line_y == self.pos.ly_compare;
                        self.stat.set_coincidence(are_equal);

                        // Request LCD STAT interrupt if conditions met
                        if self.stat.coincidence_int() && are_equal {
                            self.int.set_lcd_stat(true);
                        }

                        let next_mode = if self.pos.line_y >= 144 {
                            // Request VBlank Interrupt
                            self.int.set_vblank(true);

                            // Reset Window Line Counter in Fetcher
                            self.fetch.vblank_reset();
                            // Reset WY=LY coincidence flag
                            self.window_stat.vblank_reset();

                            if self.stat.vblank_int() {
                                // Enable Vblank LCDStat Interrupt
                                self.int.set_lcd_stat(true);
                            }

                            PpuMode::VBlank
                        } else {
                            if self.stat.oam_int() {
                                // Enable OAM LCDStat Interrupt
                                self.int.set_lcd_stat(true);
                            }

                            self.scan_state.reset();
                            PpuMode::OamScan
                        };

                        self.stat.set_mode(next_mode);
                    }
                }
                PpuMode::VBlank => {
                    if self.cycle > 456.into() {
                        self.cycle %= 456;
                        self.pos.line_y += 1;

                        // Update LY==LYC bit
                        let are_equal = self.pos.line_y == self.pos.ly_compare;
                        self.stat.set_coincidence(are_equal);

                        // Request LCD STAT interrupt if conditions met
                        if self.stat.coincidence_int() && are_equal {
                            self.int.set_lcd_stat(true);
                        }

                        if self.pos.line_y == 154 {
                            self.pos.line_y = 0;

                            if self.stat.oam_int() {
                                // Enable OAM LCDStat Interrupt
                                self.int.set_lcd_stat(true);
                            }

                            self.scan_state.reset();

                            self.stat.set_mode(PpuMode::OamScan);
                        }
                    }
                }
            }
        }
    }

    fn scan_oam(&mut self) {
        if self.scan_state.mode() == OamScanMode::Scan {
            if !self.window_stat.coincidence() && self.scan_state.count() == 0 {
                // Determine whether we should draw the window next frame
                self.window_stat
                    .set_coincidence(self.pos.line_y == self.pos.window_y);
            }

            let sprite_height = self.control.obj_size().as_u8();
            let index = self.scan_state.count();

            let attr = self.oam.attribute(index as usize);
            let line_y = self.pos.line_y + 16;

            if attr.x > 0
                && line_y >= attr.y
                && line_y < (attr.y + sprite_height)
                && !self.obj_buffer.is_full()
            {
                self.obj_buffer.add(attr);
            }

            self.scan_state.increase();
        }

        self.scan_state.next();
    }

    fn draw(&mut self, _cycle: u32) {
        use FetcherState::*;

        let iter = &mut self.obj_buffer.iter();

        let obj_attr = loop {
            match iter.flatten().next() {
                Some(attr) => {
                    if attr.x <= (self.x_pos + 8) {
                        self.fetch.back.reset();
                        self.fetch.back.pause();
                        self.fifo.pause();

                        break Some(*attr);
                    }
                }
                None => break None,
            }
        };

        if let Some(attr) = obj_attr {
            match self.fetch.obj.state {
                TileNumber => {
                    self.fetch.obj.tile.with_id(attr.tile_index);
                    self.fetch.obj.next(ToLowByteSleep);
                }
                ToLowByteSleep => self.fetch.obj.next(TileLowByte),
                TileLowByte => {
                    let obj_size = self.control.obj_size();

                    let addr = PixelFetcher::get_obj_low_addr(&attr, &self.pos, obj_size);

                    let byte = self.read_byte(addr);
                    self.fetch.obj.tile.with_low_byte(byte);

                    self.fetch.obj.next(ToHighByteSleep);
                }
                ToHighByteSleep => self.fetch.obj.next(TileHighByte),
                TileHighByte => {
                    let obj_size = self.control.obj_size();

                    let addr = PixelFetcher::get_obj_low_addr(&attr, &self.pos, obj_size);

                    let byte = self.read_byte(addr + 1);
                    self.fetch.obj.tile.with_high_byte(byte);

                    self.fetch.obj.next(ToFifoSleep);
                }
                ToFifoSleep => self.fetch.obj.next(SendToFifoOne),
                SendToFifoOne => {
                    // Load into Fifo
                    let (high, low) = self
                        .fetch
                        .obj
                        .tile
                        .bytes()
                        .expect("Failed to unwrap Tile bytes");

                    let tbpp = Pixels::from_bytes(high, low);

                    let palette = match attr.flags.palette() {
                        ObjectPaletteId::Zero => self.monochrome.obj_palette_0,
                        ObjectPaletteId::One => self.monochrome.obj_palette_1,
                    };

                    let end = Pixels::PIXEL_COUNT - self.fifo.obj.len();
                    let start = Pixels::PIXEL_COUNT - end;

                    let x_flip = attr.flags.x_flip();

                    for i in start..Pixels::PIXEL_COUNT {
                        let x = if x_flip { 7 - i } else { i };

                        let priority = attr.flags.priority();
                        let shade = palette.shade(tbpp.shade_id(x));

                        let fifo_info = ObjectFifoInfo {
                            shade,
                            palette,
                            priority,
                        };

                        self.fifo.obj.push_back(fifo_info);
                    }

                    self.fetch.back.resume();
                    self.fifo.resume();
                    self.obj_buffer.remove(&attr);

                    self.fetch.obj.next(SendToFifoTwo);
                }
                SendToFifoTwo => self.fetch.obj.reset(),
            }
        }

        if self.control.window_enabled()
            && !self.window_stat.should_draw()
            && self.window_stat.coincidence()
            && self.x_pos >= self.pos.window_x - 7
        {
            self.window_stat.set_should_draw(true);
            self.fetch.back.reset();
            self.fetch.x_pos = 0;
            self.fifo.back.clear();
        }

        if self.fetch.back.is_enabled() {
            match self.fetch.back.state {
                TileNumber => {
                    let x_pos = self.fetch.x_pos;

                    self.fetch
                        .back
                        .should_render_window(self.window_stat.should_draw());

                    let addr = self.fetch.bg_tile_num_addr(&self.control, &self.pos, x_pos);

                    let id = self.read_byte(addr);
                    self.fetch.back.tile.with_id(id);

                    // Move on to the Next state in 2 T-cycles
                    self.fetch.back.next(ToLowByteSleep);
                }
                ToLowByteSleep => self.fetch.back.next(TileLowByte),
                TileLowByte => {
                    let addr = self.fetch.bg_byte_low_addr(&self.control, &self.pos);

                    let low = self.read_byte(addr);
                    self.fetch.back.tile.with_low_byte(low);

                    self.fetch.back.next(ToHighByteSleep);
                }
                ToHighByteSleep => self.fetch.back.next(TileHighByte),
                TileHighByte => {
                    let addr = self.fetch.bg_byte_low_addr(&self.control, &self.pos);

                    let high = self.read_byte(addr + 1);
                    self.fetch.back.tile.with_high_byte(high);

                    self.fetch.back.next(ToFifoSleep);
                }
                ToFifoSleep => self.fetch.back.next(SendToFifoOne),
                SendToFifoOne => {
                    self.fetch.back.next(SendToFifoTwo);
                }
                SendToFifoTwo => {
                    let palette = &self.monochrome.bg_palette;
                    self.fetch.send_to_fifo(&mut self.fifo, palette);
                    self.fetch.x_pos += 1;

                    self.fetch.back.next(TileNumber);
                    self.fetch.back.tile = Default::default();
                }
            }
        }

        if self.fifo.is_enabled() {
            use RenderPriority::*;

            // Handle Background Pixel and Sprite FIFO
            let bg_enabled = self.control.bg_win_enabled();
            let i0_colour = self.monochrome.bg_palette.i0_colour();

            // FIXME: Is this the correct behaviour
            let rgba_opt = self.fifo.back.pop_front().map(|bg_info| {
                let bg_shade = if bg_enabled { bg_info.shade } else { i0_colour };

                match self.fifo.obj.pop_front() {
                    Some(obj_info) => match (obj_info.shade, obj_info.priority) {
                        (Some(obj_shade), BackgroundAndWindow) => match bg_info.shade {
                            GrayShade::White => obj_shade.into_rgba(),
                            _ => bg_shade.into_rgba(),
                        },
                        (Some(obj_shade), Object) => obj_shade.into_rgba(),
                        (None, _) => bg_shade.into_rgba(),
                    },
                    None => bg_shade.into_rgba(),
                }
            });

            if let Some(rgba) = rgba_opt.as_ref() {
                let y = self.pos.line_y as usize;
                let x = self.x_pos as usize;

                let i = (GB_WIDTH * 4) * y + (x * 4);
                self.frame_buf[i..(i + rgba.len())].copy_from_slice(rgba);

                self.x_pos += 1;
            }
        }
    }

    fn reset(&mut self) {
        // FIXME: Discover what actually is supposed to be reset here

        self.scan_state = Default::default();
        self.cycle = Cycle::new(0);

        self.x_pos = 0;
        self.window_stat = Default::default();
        self.stat.set_mode(PpuMode::OamScan);
        self.pos.line_y = 0;

        self.fetch.back.reset();
        self.fetch.obj.reset();
        self.obj_buffer.clear();
    }

    pub fn copy_to_gui(&self, frame: &mut [u8]) {
        frame.copy_from_slice(self.frame_buf.as_ref());
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            vram: Box::new([0u8; VRAM_SIZE]),
            cycle: Cycle::new(0),
            frame_buf: Box::new([0; GB_WIDTH * GB_HEIGHT * 4]),
            int: Default::default(),
            control: Default::default(),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: Default::default(),
            oam: Default::default(),
            scan_state: Default::default(),
            fetch: Default::default(),
            fifo: Default::default(),
            obj_buffer: Default::default(),
            window_stat: Default::default(),
            x_pos: 0,
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

#[derive(Debug, Clone, Copy, Default)]
pub struct ScreenPosition {
    pub scroll_y: u8,
    pub scroll_x: u8,
    pub line_y: u8,
    pub ly_compare: u8,
    pub window_y: u8,
    pub window_x: u8,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Monochrome {
    pub bg_palette: BackgroundPalette,
    pub obj_palette_0: ObjectPalette,
    pub obj_palette_1: ObjectPalette,
}

#[derive(Debug, Clone)]
pub struct ObjectAttributeTable {
    buf: Box<[u8; OAM_SIZE]>,
}

impl ObjectAttributeTable {
    pub fn read_byte(&self, addr: u16) -> u8 {
        let index = (addr - 0xFE00) as usize;
        self.buf[index]
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
        let index = (addr - 0xFE00) as usize;
        self.buf[index] = byte;
    }

    pub fn attribute(&self, index: usize) -> ObjectAttribute {
        let start = index * 4;

        let slice: &[u8; 4] = self.buf[start..(start + 4)]
            .try_into()
            .expect("Could not interpret &[u8] as a &[u8; 4]");

        slice.into()
    }
}

impl Default for ObjectAttributeTable {
    fn default() -> Self {
        Self {
            buf: Box::new([0; OAM_SIZE]),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ObjectAttribute {
    y: u8,
    x: u8,
    tile_index: u8,
    flags: ObjectFlags,
}

impl From<[u8; 4]> for ObjectAttribute {
    fn from(bytes: [u8; 4]) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_index: bytes[2],
            flags: bytes[3].into(),
        }
    }
}

impl<'a> From<&'a [u8; 4]> for ObjectAttribute {
    fn from(bytes: &'a [u8; 4]) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_index: bytes[2],
            flags: bytes[3].into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ObjectBuffer {
    buf: [Option<ObjectAttribute>; OBJECT_LIMIT],
    len: usize,
}

impl ObjectBuffer {
    pub fn iter(&self) -> std::slice::Iter<'_, Option<ObjectAttribute>> {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a ObjectBuffer {
    type Item = &'a Option<ObjectAttribute>;

    type IntoIter = std::slice::Iter<'a, Option<ObjectAttribute>>;

    fn into_iter(self) -> Self::IntoIter {
        self.buf.iter()
    }
}

impl<'a> IntoIterator for &'a mut ObjectBuffer {
    type Item = &'a Option<ObjectAttribute>;

    type IntoIter = std::slice::Iter<'a, Option<ObjectAttribute>>;

    fn into_iter(self) -> Self::IntoIter {
        self.buf.iter()
    }
}

impl ObjectBuffer {
    pub fn is_full(&self) -> bool {
        self.len == OBJECT_LIMIT
    }

    pub fn clear(&mut self) {
        self.buf = [Default::default(); 10];
        self.len = 0;
    }

    pub fn add(&mut self, attr: ObjectAttribute) {
        self.buf[self.len] = Some(attr);
        self.len += 1;
    }

    pub fn remove(&mut self, attr: &ObjectAttribute) {
        let maybe_index = self.buf.iter().position(|maybe_attr| match maybe_attr {
            Some(other_attr) => attr == other_attr,
            None => false,
        });

        if let Some(i) = maybe_index {
            self.buf[i] = None;
        }
    }
}

impl Default for ObjectBuffer {
    fn default() -> Self {
        Self {
            buf: [Default::default(); OBJECT_LIMIT],
            len: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct PixelFetcher {
    x_pos: u8,
    back: BackgroundFetcher,
    obj: ObjectFetcher,
}

impl PixelFetcher {
    pub fn hblank_reset(&mut self) {
        self.back.hblank_reset();
        self.obj.hblank_reset();
        self.x_pos = 0;
    }

    pub fn vblank_reset(&mut self) {
        self.back.vblank_reset();
    }

    fn bg_tile_num_addr(&self, control: &LCDControl, pos: &ScreenPosition, x_pos: u8) -> u16 {
        let line_y = pos.line_y;
        let scroll_y = pos.scroll_y;
        let scroll_x = pos.scroll_x;
        let is_window = self.back.is_window_tile();

        // Determine which tile map is being used
        let tile_map = if is_window {
            control.win_tile_map_addr()
        } else {
            control.bg_tile_map_addr()
        };
        let tile_map_addr = tile_map.into_address();

        // Both Offsets are used to offset the tile map address we found above
        // Offsets are ANDed wih 0x3FF so that we stay in bounds of tile map memory
        // TODO: Is this necessary / important in other fetcher modes?

        let scx_offset = if is_window { 0u16 } else { scroll_x as u16 / 8 } & 0x1F;
        let y_offset = if is_window {
            self.back.window_line.count() as u16 / 8
        } else {
            ((line_y as u16 + scroll_y as u16) & 0xFF) / 8
        } * 32;

        let x_offset = x_pos as u16 + scx_offset;

        tile_map_addr + ((x_offset + y_offset) & 0x3FF)
    }

    fn bg_byte_low_addr(&mut self, control: &LCDControl, pos: &ScreenPosition) -> u16 {
        let line_y = pos.line_y;
        let scroll_y = pos.scroll_y;
        let is_window = self.back.is_window_tile();

        let id = self.back.tile.id.expect("Tile Number unexpectedly missing");

        let tile_data_addr = match control.tile_data_addr() {
            TileDataAddress::X8800 => 0x9000u16.wrapping_add((id as i8).wrapping_mul(16) as u16),
            TileDataAddress::X8000 => 0x8000 + (id as u16).wrapping_mul(16),
        };

        let offset = if is_window {
            self.back.window_line.count() % 8
        } else {
            (line_y + scroll_y) % 8
        } * 2;

        tile_data_addr + offset as u16
    }

    fn send_to_fifo(&self, fifo: &mut FifoRenderer, palette: &BackgroundPalette) {
        let (high, low) = self.back.tile.bytes().expect("Failed to unwrap Tile bytes");

        let tbpp = Pixels::from_bytes(high, low);

        if fifo.back.is_empty() {
            for x in 0..Pixels::PIXEL_COUNT {
                let shade = palette.shade(tbpp.shade_id(x));

                let fifo_info = BackgroundFifoInfo { shade };
                fifo.back.push_back(fifo_info);
            }
        }
    }

    pub fn get_obj_low_addr(attr: &ObjectAttribute, pos: &ScreenPosition, size: ObjectSize) -> u16 {
        let line_y = pos.line_y;
        let scroll_y = pos.scroll_y;

        let tile_number = match size {
            ObjectSize::Eight => attr.tile_index,
            ObjectSize::Sixteen => attr.tile_index & !0x01,
        };

        let offset = 2 * if attr.flags.y_flip() {
            (size.as_u8() - 1) - (line_y + scroll_y) % 8
        } else {
            (line_y + scroll_y) % 8
        };

        0x8000 + (tile_number as u16 * 16) + offset as u16
    }
}

trait Fetcher {
    fn next(&mut self, state: FetcherState);
    fn reset(&mut self);
    fn hblank_reset(&mut self);
}

#[derive(Debug, Clone, Copy)]
struct BackgroundFetcher {
    state: FetcherState,
    tile: TileBuilder,
    window_line: WindowLineCounter,
    is_window_title: bool,
    enabled: bool,
}

impl BackgroundFetcher {
    fn should_render_window(&mut self, value: bool) {
        self.is_window_title = value;
    }

    fn is_window_tile(&self) -> bool {
        self.is_window_title
    }

    fn pause(&mut self) {
        self.enabled = false;
    }

    fn resume(&mut self) {
        self.enabled = true;
    }

    fn is_enabled(&self) -> bool {
        self.enabled
    }

    fn vblank_reset(&mut self) {
        self.window_line.vblank_reset();
    }
}

impl Fetcher for BackgroundFetcher {
    fn next(&mut self, state: FetcherState) {
        self.state = state
    }

    fn reset(&mut self) {
        self.state = Default::default();
        self.tile = Default::default();
    }

    fn hblank_reset(&mut self) {
        self.reset();

        self.is_window_title = false;

        self.enabled = true;
    }
}

impl Default for BackgroundFetcher {
    fn default() -> Self {
        Self {
            state: Default::default(),
            tile: Default::default(),
            is_window_title: Default::default(),
            window_line: Default::default(),
            enabled: true,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct ObjectFetcher {
    state: FetcherState,
    tile: TileBuilder,
}

impl Fetcher for ObjectFetcher {
    fn next(&mut self, state: FetcherState) {
        self.state = state
    }

    fn reset(&mut self) {
        self.state = Default::default();
        self.tile = Default::default();
    }

    fn hblank_reset(&mut self) {
        self.state = Default::default();
        self.tile = Default::default();
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct WindowLineCounter {
    count: u8,
}

impl WindowLineCounter {
    pub fn increment(&mut self) {
        self.count += 1;
    }

    pub fn vblank_reset(&mut self) {
        self.count = 0;
    }

    pub fn count(&self) -> u8 {
        self.count
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FetcherState {
    TileNumber,
    ToLowByteSleep,
    TileLowByte,
    ToHighByteSleep,
    TileHighByte,
    ToFifoSleep,
    SendToFifoOne,
    SendToFifoTwo,
}

impl Default for FetcherState {
    fn default() -> Self {
        Self::TileNumber
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct BackgroundFifoInfo {
    shade: GrayShade,
}

#[derive(Debug, Clone, Copy, Default)]
struct ObjectFifoInfo {
    shade: Option<GrayShade>,
    palette: ObjectPalette,
    priority: RenderPriority,
}

// FIXME: Fifo Registers have a known size. Are heap allocations
// really necessary here?
#[derive(Debug, Clone)]
struct FifoRenderer {
    back: VecDeque<BackgroundFifoInfo>,
    obj: VecDeque<ObjectFifoInfo>,
    enabled: bool,
}

impl FifoRenderer {
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    pub fn pause(&mut self) {
        self.enabled = false;
    }

    pub fn resume(&mut self) {
        self.enabled = true;
    }
}

impl Default for FifoRenderer {
    fn default() -> Self {
        Self {
            back: VecDeque::with_capacity(8),
            obj: VecDeque::with_capacity(8),
            enabled: true,
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

    pub fn with_low_byte(&mut self, data: u8) {
        self.low = Some(data);
    }

    pub fn with_high_byte(&mut self, data: u8) {
        self.high = Some(data);
    }

    pub fn bytes(&self) -> Option<(u8, u8)> {
        self.high.zip(self.low)
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct OamScanState {
    count: u8,
    mode: OamScanMode,
}

impl OamScanState {
    pub fn increase(&mut self) {
        self.count += 1;
        self.count %= 40;
    }

    pub fn reset(&mut self) {
        self.count = Default::default();
        self.mode = Default::default();
    }

    pub fn count(&self) -> u8 {
        self.count
    }

    pub fn mode(&self) -> OamScanMode {
        self.mode
    }

    pub fn next(&mut self) {
        use OamScanMode::*;

        self.mode = match self.mode {
            Scan => Sleep,
            Sleep => Scan,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum OamScanMode {
    Scan,
    Sleep,
}

impl Default for OamScanMode {
    fn default() -> Self {
        Self::Scan
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct WindowStatus {
    /// This will be true if WY == LY at any point in the frame thus far
    coincidence: bool,
    /// This will be true if the conditions which tell the PPU to start
    /// drawing from the window tile map is true
    should_draw: bool,
}

impl WindowStatus {
    pub fn should_draw(&self) -> bool {
        self.should_draw
    }

    pub fn coincidence(&self) -> bool {
        self.coincidence
    }

    pub fn set_should_draw(&mut self, value: bool) {
        self.should_draw = value;
    }

    pub fn set_coincidence(&mut self, value: bool) {
        self.coincidence = value;
    }

    fn hblank_reset(&mut self) {
        self.should_draw = false;
    }

    fn vblank_reset(&mut self) {
        self.coincidence = false;
    }
}
