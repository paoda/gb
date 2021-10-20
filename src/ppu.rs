use crate::bus::BusIo;
use crate::Cycle;
use crate::GB_HEIGHT;
use crate::GB_WIDTH;
use dma::DirectMemoryAccess;
use std::collections::VecDeque;
use std::convert::TryInto;
pub(crate) use types::PpuMode;
use types::{
    BackgroundPalette, GrayShade, LCDControl, LCDStatus, ObjectFlags, ObjectPalette,
    ObjectPaletteKind, ObjectSize, Pixels, RenderPriority, TileDataAddress,
};

mod dma;
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

#[derive(Debug)]
pub struct Ppu {
    pub(crate) int: Interrupt,
    /// 0xFF40 | LCDC - LCD Control
    pub(crate) ctrl: LCDControl,
    /// 0xFF41 | STAT - LCD Status
    pub(crate) stat: LCDStatus,
    pub(crate) monochrome: Monochrome,
    pub(crate) pos: ScreenPosition,
    vram: Box<[u8; VRAM_SIZE]>,
    pub(crate) oam: ObjectAttributeTable,
    pub(crate) dma: DirectMemoryAccess,
    scan_cycle: Cycle,
    fetch: PixelFetcher,
    fifo: PixelFifo,
    obj_buffer: ObjectBuffer,
    frame_buf: Box<[u8; GB_WIDTH * GB_HEIGHT * 4]>,
    window_stat: WindowStatus,

    scanline_start: bool,
    to_discard: u8,

    x_pos: u8,
    cycle: Cycle,
}

impl BusIo for Ppu {
    fn read_byte(&self, addr: u16) -> u8 {
        self.vram[addr as usize - PPU_START_ADDRESS]
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.vram[addr as usize - PPU_START_ADDRESS] = byte;
    }
}

impl Ppu {
    pub(crate) fn tick(&mut self) {
        self.cycle += 1;

        if !self.ctrl.lcd_enabled() {
            return;
        }

        match self.stat.mode() {
            PpuMode::OamScan => {
                if self.cycle >= 80 {
                    self.stat.set_mode(PpuMode::Drawing);
                }

                self.scan_oam();
            }
            PpuMode::Drawing => {
                if self.ctrl.lcd_enabled() {
                    // Only Draw when the LCD Is Enabled
                    self.draw(self.cycle);
                } else {
                    self.reset();
                }

                if self.x_pos == 160 {
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
                    self.scanline_start = true;
                    self.to_discard = 0;

                    self.fetch.hblank_reset();
                    self.window_stat.hblank_reset();
                    self.obj_buffer.clear();

                    self.fifo.back.clear();
                    self.fifo.obj.clear();

                    self.stat.set_mode(PpuMode::HBlank);
                }
            }
            PpuMode::HBlank => {
                // This mode will always end at 456 cycles

                if self.cycle >= 456 {
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

                        self.scan_cycle = Default::default();
                        PpuMode::OamScan
                    };

                    self.stat.set_mode(next_mode);
                }
            }
            PpuMode::VBlank => {
                if self.cycle > 456 {
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

                        self.scan_cycle = Default::default();
                        self.stat.set_mode(PpuMode::OamScan);
                    }
                }
            }
        }
    }

    fn scan_oam(&mut self) {
        if self.scan_cycle % 2 == 0 {
            if self.dma.is_active() {
                return;
            }

            if !self.window_stat.coincidence() && self.scan_cycle == 0 {
                self.window_stat
                    .set_coincidence(self.pos.line_y == self.pos.window_y);
            }

            let obj_height = self.ctrl.obj_size().size();
            let attr = self.oam.attribute(self.scan_cycle as usize / 2);
            let line_y = self.pos.line_y + 16;

            if attr.x > 0
                && line_y >= attr.y
                && line_y < (attr.y + obj_height)
                && !self.obj_buffer.is_full()
            {
                self.obj_buffer.add(attr);
            }
        }
        self.scan_cycle += 1;
    }

    fn draw(&mut self, _cycle: Cycle) {
        use FetcherState::*;

        let mut iter = self.obj_buffer.iter_mut();
        let default = &mut None;

        let obj_attr = loop {
            match iter.next() {
                Some(attr_opt) => {
                    if let Some(attr) = attr_opt {
                        if attr.x <= (self.x_pos + 8) {
                            self.fetch.back.reset();
                            self.fetch.back.pause();
                            self.fifo.pause();

                            break attr_opt;
                        }
                    }
                }
                None => break default,
            }
        };

        if let Some(attr) = obj_attr {
            match self.fetch.obj.state {
                TileNumber => {
                    self.fetch.obj.tile.with_id(attr.tile_index);
                    self.fetch.obj.next(SleepOne);
                }
                SleepOne => self.fetch.obj.next(TileLow),
                TileLow => {
                    let obj_size = self.ctrl.obj_size();

                    let addr = PixelFetcher::get_obj_addr(attr, &self.pos, obj_size);

                    let byte = self.read_byte(addr);
                    self.fetch.obj.tile.with_low_byte(byte);

                    self.fetch.obj.next(SleepTwo);
                }
                SleepTwo => self.fetch.obj.next(TileHigh),
                TileHigh => {
                    let obj_size = self.ctrl.obj_size();

                    let addr = PixelFetcher::get_obj_addr(attr, &self.pos, obj_size);

                    let byte = self.read_byte(addr + 1);
                    self.fetch.obj.tile.with_high_byte(byte);

                    self.fetch.obj.next(SleepThree);
                }
                SleepThree => self.fetch.obj.next(ToFifoOne),
                ToFifoOne => {
                    // Load into Fifo
                    let (high, low) = self
                        .fetch
                        .obj
                        .tile
                        .bytes()
                        .expect("Tile high & low bytes are present");

                    let tbpp = Pixels::from_bytes(high, low);

                    let palette_kind = attr.flags.palette();
                    let x_flip = attr.flags.x_flip();

                    let pixel_count = (attr.x - self.x_pos) as usize;
                    let start = self.fifo.obj.len();

                    for i in start..pixel_count {
                        let x = if x_flip { 7 - i } else { i };

                        let priority = attr.flags.priority();
                        let shade_id = tbpp.shade_id(x);

                        let fifo_info = ObjPixelProperty {
                            shade_id,
                            palette_kind,
                            priority,
                        };

                        self.fifo.obj.push_back(fifo_info);
                    }

                    self.fetch.back.resume();
                    self.fifo.resume();
                    let _ = std::mem::take(obj_attr);

                    self.fetch.obj.next(ToFifoTwo);
                }
                ToFifoTwo => self.fetch.obj.reset(),
            }
        }

        if self.ctrl.window_enabled()
            && !self.window_stat.should_draw()
            && self.window_stat.coincidence()
            && self.x_pos as i16 >= self.pos.window_x as i16 - 7
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

                    let addr = self.fetch.bg_tile_num_addr(&self.ctrl, &self.pos, x_pos);

                    let id = self.read_byte(addr);
                    self.fetch.back.tile.with_id(id);

                    // Move on to the Next state in 2 T-cycles
                    self.fetch.back.next(SleepOne);
                }
                SleepOne => self.fetch.back.next(TileLow),
                TileLow => {
                    let addr = self.fetch.bg_byte_addr(&self.ctrl, &self.pos);

                    let low = self.read_byte(addr);
                    self.fetch.back.tile.with_low_byte(low);

                    self.fetch.back.next(SleepTwo);
                }
                SleepTwo => self.fetch.back.next(TileHigh),
                TileHigh => {
                    let addr = self.fetch.bg_byte_addr(&self.ctrl, &self.pos);

                    let high = self.read_byte(addr + 1);
                    self.fetch.back.tile.with_high_byte(high);

                    self.fetch.back.next(SleepThree);
                }
                SleepThree => self.fetch.back.next(ToFifoOne),
                ToFifoOne => {
                    self.fetch.back.next(ToFifoTwo);
                }
                ToFifoTwo => {
                    if let Ok(()) = self.fetch.send_to_fifo(&mut self.fifo) {
                        self.fetch.x_pos += 1;
                        self.fetch.back.next(TileNumber);
                        self.fetch.back.tile = Default::default();
                    }
                }
            }
        }

        if self.fifo.is_enabled() {
            if self.x_pos == 0 && self.scanline_start {
                self.to_discard = self.pos.scroll_x % 8;
                self.scanline_start = false;
            }

            if self.to_discard > 0 && !self.fifo.back.is_empty() {
                let _ = self.fifo.back.pop_front();
                self.to_discard -= 1;

                // Delay the PPU by one cycle
                return;
            }

            // Handle Background Pixel and Sprite FIFO
            if let Some(rgba) = self.clock_fifo().map(GrayShade::into_rgba) {
                let y = self.pos.line_y as usize;
                let x = self.x_pos as usize;

                let i = (GB_WIDTH * 4) * y + (x * 4);
                self.frame_buf[i..(i + rgba.len())].copy_from_slice(&rgba);

                self.x_pos += 1;
            }
        }
    }

    fn reset(&mut self) {
        self.pos.line_y = 0;
        self.stat.set_mode(PpuMode::HBlank);

        let mut blank = WHITE.repeat(self.frame_buf.len() / 4);
        self.frame_buf.swap_with_slice(&mut blank);
    }

    #[inline]
    pub(crate) fn frame_buf(&self) -> &[u8; GB_HEIGHT * GB_WIDTH * 4] {
        &self.frame_buf
    }

    fn clock_fifo(&mut self) -> Option<GrayShade> {
        use ObjectPaletteKind::*;
        use RenderPriority::*;

        let obj_palette_0 = &self.monochrome.obj_palette_0;
        let obj_palette_1 = &self.monochrome.obj_palette_1;

        match self.fifo.back.pop_front() {
            Some(bg_pixel) => match self.fifo.obj.pop_front() {
                Some(obj_pixel) if self.ctrl.obj_enabled() => match obj_pixel.priority {
                    Object | BackgroundAndWindow if obj_pixel.shade_id == 0 => {
                        Some(self.bg_pixel(bg_pixel.shade_id))
                    }
                    BackgroundAndWindow if bg_pixel.shade_id != 0 => {
                        Some(self.bg_pixel(bg_pixel.shade_id))
                    }
                    Object | BackgroundAndWindow => {
                        let maybe_sprite = match obj_pixel.palette_kind {
                            Zero => obj_palette_0.shade(obj_pixel.shade_id),
                            One => obj_palette_1.shade(obj_pixel.shade_id),
                        };

                        let sprite = maybe_sprite
                            .expect("Sprite w/ a colour id of 0 has already been handled");
                        Some(sprite)
                    }
                },
                _ => Some(self.bg_pixel(bg_pixel.shade_id)),
            },
            None => None,
        }
    }

    fn bg_pixel(&self, shade_id: u8) -> GrayShade {
        let bg_palette = &self.monochrome.bg_palette;

        if self.ctrl.bg_win_enabled() {
            bg_palette.shade(shade_id)
        } else {
            bg_palette.shade(0)
        }
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            vram: Box::new([0u8; VRAM_SIZE]),
            cycle: Default::default(),
            frame_buf: Box::new([0; GB_WIDTH * GB_HEIGHT * 4]),
            int: Default::default(),
            ctrl: Default::default(),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: Default::default(),
            oam: Default::default(),
            scan_cycle: Default::default(),
            fetch: Default::default(),
            fifo: Default::default(),
            obj_buffer: Default::default(),
            window_stat: Default::default(),
            dma: Default::default(),
            x_pos: 0,
            scanline_start: true,
            to_discard: Default::default(),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Interrupt {
    _vblank: bool,
    _lcd_stat: bool,
}

impl Interrupt {
    pub(crate) fn vblank(&self) -> bool {
        self._vblank
    }

    pub(crate) fn set_vblank(&mut self, enabled: bool) {
        self._vblank = enabled;
    }

    pub(crate) fn lcd_stat(&self) -> bool {
        self._lcd_stat
    }

    pub(crate) fn set_lcd_stat(&mut self, enabled: bool) {
        self._lcd_stat = enabled;
    }
}

#[derive(Debug, Default)]
pub(crate) struct ScreenPosition {
    /// 0xFF42 | SCY - Scroll Y
    pub(crate) scroll_y: u8,
    /// 0xFF43 | SCX - Scroll X
    pub(crate) scroll_x: u8,
    /// 0xFF44 | LY - LCD Y Coordinate
    pub(crate) line_y: u8,
    /// 0xFF45 | LYC - LY Compare
    pub(crate) ly_compare: u8,
    /// 0xFF4A | WY - Window Y Position
    pub(crate) window_y: u8,
    /// 0xFF4B | WX - Window X Position
    pub(crate) window_x: u8,
}

#[derive(Debug, Default)]
pub(crate) struct Monochrome {
    /// 0xFF47 | BGP - Background Palette Data
    pub(crate) bg_palette: BackgroundPalette,
    /// 0xFF48 | OBP0 - Object Palette 0 Data
    pub(crate) obj_palette_0: ObjectPalette,
    /// 0xFF49 | OBP1 - Object Palette 1 Data
    pub(crate) obj_palette_1: ObjectPalette,
}

#[derive(Debug)]
pub(crate) struct ObjectAttributeTable {
    buf: Box<[u8; OAM_SIZE]>,
}

impl BusIo for ObjectAttributeTable {
    fn read_byte(&self, addr: u16) -> u8 {
        let index = (addr - 0xFE00) as usize;
        self.buf[index]
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        let index = (addr - 0xFE00) as usize;
        self.buf[index] = byte;
    }
}

impl ObjectAttributeTable {
    fn attribute(&self, index: usize) -> ObjectAttribute {
        let start = index * 4;

        let slice: &[u8; 4] = self.buf[start..(start + 4)]
            .try_into()
            .expect("TryInto trait called on a &[u8; 4]");

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
struct ObjectAttribute {
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

#[derive(Debug)]
struct ObjectBuffer {
    inner: [Option<ObjectAttribute>; OBJECT_LIMIT],
    len: usize,
}

impl ObjectBuffer {
    fn is_full(&self) -> bool {
        self.len == OBJECT_LIMIT
    }

    fn clear(&mut self) {
        self.inner = [Default::default(); 10];
        self.len = 0;
    }

    fn add(&mut self, attr: ObjectAttribute) {
        self.inner[self.len] = Some(attr);
        self.len += 1;
    }

    #[inline]
    fn iter_mut(&mut self) -> std::slice::IterMut<'_, Option<ObjectAttribute>> {
        self.inner.iter_mut()
    }
}

impl Default for ObjectBuffer {
    fn default() -> Self {
        Self {
            inner: [Default::default(); OBJECT_LIMIT],
            len: Default::default(),
        }
    }
}

#[derive(Debug, Default)]
struct PixelFetcher {
    x_pos: u8,
    back: BackgroundFetcher,
    obj: ObjectFetcher,
}

impl PixelFetcher {
    fn hblank_reset(&mut self) {
        self.back.hblank_reset();
        self.obj.hblank_reset();
        self.x_pos = 0;
    }

    fn vblank_reset(&mut self) {
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

        let scx_offset = if is_window { 0 } else { scroll_x / 8 };
        let y_offset = if is_window {
            self.back.window_line.count() as u16 / 8
        } else {
            ((line_y as u16 + scroll_y as u16) & 0xFF) / 8
        };

        let x_offset = (scx_offset + x_pos) & 0x1F;
        let offset = (32 * y_offset) + (x_offset as u16);

        tile_map_addr + (offset & 0x3FF)
    }

    fn bg_byte_addr(&mut self, control: &LCDControl, pos: &ScreenPosition) -> u16 {
        let line_y = pos.line_y;
        let scroll_y = pos.scroll_y;
        let is_window = self.back.is_window_tile();

        let id = self.back.tile.id.expect("Tile Number is present");

        let tile_data_addr = match control.tile_data_addr() {
            TileDataAddress::X8800 => 0x9000u16.wrapping_add((id as i8 as i16 * 16) as u16),
            TileDataAddress::X8000 => 0x8000 + (id as u16 * 16),
        };

        let offset = if is_window {
            self.back.window_line.count() as u16 % 8
        } else {
            (line_y as u16 + scroll_y as u16) % 8
        };

        tile_data_addr + (offset * 2)
    }

    fn send_to_fifo(&self, fifo: &mut PixelFifo) -> Result<(), ()> {
        if !fifo.back.is_empty() {
            return Err(());
        }

        let (high, low) = self
            .back
            .tile
            .bytes()
            .expect("Tile high & low bytes are present");

        let tbpp = Pixels::from_bytes(high, low);

        for x in 0..Pixels::PIXEL_COUNT {
            let shade_id = tbpp.shade_id(x);

            let fifo_info = BgPixelProperty { shade_id };
            fifo.back.push_back(fifo_info);
        }

        Ok(())
    }

    fn get_obj_addr(attr: &ObjectAttribute, pos: &ScreenPosition, size: ObjectSize) -> u16 {
        let line_y = pos.line_y;

        // TODO: Why is the offset 14 and 30 respectively?
        let (id, flip_offset) = match size {
            ObjectSize::Eight => (attr.tile_index, 14),
            ObjectSize::Sixteen => (attr.tile_index & !0x01, 30),
        };

        let offset = 2 * (line_y - (attr.y - 16));

        let final_offset = if attr.flags.y_flip() {
            flip_offset - offset
        } else {
            offset
        };

        0x8000 + (id as u16 * 16) + final_offset as u16
    }
}

trait Fetcher {
    fn next(&mut self, state: FetcherState);
    fn reset(&mut self);
    fn hblank_reset(&mut self);
}

#[derive(Debug)]
struct BackgroundFetcher {
    state: FetcherState,
    tile: TileBuilder,
    window_line: WindowLineCounter,
    is_window_tile: bool,
    enabled: bool,
}

impl BackgroundFetcher {
    fn should_render_window(&mut self, value: bool) {
        self.is_window_tile = value;
    }

    fn is_window_tile(&self) -> bool {
        self.is_window_tile
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

        self.is_window_tile = false;

        self.enabled = true;
    }
}

impl Default for BackgroundFetcher {
    fn default() -> Self {
        Self {
            state: Default::default(),
            tile: Default::default(),
            is_window_tile: Default::default(),
            window_line: Default::default(),
            enabled: true,
        }
    }
}

#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
struct WindowLineCounter {
    count: u8,
}

impl WindowLineCounter {
    fn increment(&mut self) {
        self.count += 1;
    }

    fn vblank_reset(&mut self) {
        self.count = 0;
    }

    fn count(&self) -> u8 {
        self.count
    }
}

#[derive(Debug, Clone, Copy)]
enum FetcherState {
    TileNumber,
    SleepOne,
    TileLow,
    SleepTwo,
    TileHigh,
    SleepThree,
    ToFifoOne,
    ToFifoTwo,
}

impl Default for FetcherState {
    fn default() -> Self {
        Self::TileNumber
    }
}

#[derive(Debug, Default)]
struct BgPixelProperty {
    shade_id: u8,
}

#[derive(Debug, Default)]
struct ObjPixelProperty {
    shade_id: u8,
    palette_kind: ObjectPaletteKind,
    priority: RenderPriority,
}

// FIXME: Fifo Registers have a known size. Are heap allocations
// really necessary here?
#[derive(Debug)]
struct PixelFifo {
    back: VecDeque<BgPixelProperty>,
    obj: VecDeque<ObjPixelProperty>,
    enabled: bool,
}

impl PixelFifo {
    fn is_enabled(&self) -> bool {
        self.enabled
    }

    fn pause(&mut self) {
        self.enabled = false;
    }

    fn resume(&mut self) {
        self.enabled = true;
    }
}

impl Default for PixelFifo {
    fn default() -> Self {
        Self {
            back: VecDeque::with_capacity(8),
            obj: VecDeque::with_capacity(8),
            enabled: true,
        }
    }
}

#[derive(Debug, Default)]
struct TileBuilder {
    id: Option<u8>,
    low: Option<u8>,
    high: Option<u8>,
}

impl TileBuilder {
    fn with_id(&mut self, id: u8) {
        self.id = Some(id);
    }

    fn with_low_byte(&mut self, data: u8) {
        self.low = Some(data);
    }

    fn with_high_byte(&mut self, data: u8) {
        self.high = Some(data);
    }

    fn bytes(&self) -> Option<(u8, u8)> {
        self.high.zip(self.low)
    }
}

#[derive(Debug, Default)]
struct WindowStatus {
    /// This will be true if WY == LY at any point in the frame thus far
    coincidence: bool,
    /// This will be true if the conditions which tell the PPU to start
    /// drawing from the window tile map is true
    should_draw: bool,
}

impl WindowStatus {
    fn should_draw(&self) -> bool {
        self.should_draw
    }

    fn coincidence(&self) -> bool {
        self.coincidence
    }

    fn set_should_draw(&mut self, value: bool) {
        self.should_draw = value;
    }

    fn set_coincidence(&mut self, value: bool) {
        self.coincidence = value;
    }

    fn hblank_reset(&mut self) {
        self.should_draw = false;
    }

    fn vblank_reset(&mut self) {
        self.coincidence = false;
    }
}
