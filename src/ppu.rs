use crate::bus::BusIo;
use crate::Cycle;
use crate::GB_HEIGHT;
use crate::GB_WIDTH;
use dma::DirectMemoryAccess;
use std::collections::VecDeque;
pub(crate) use types::PpuMode;
use types::{
    BackgroundPalette, GrayShade, LCDControl, LCDStatus, ObjectFlags, ObjectPalette,
    ObjectPaletteKind, ObjectSize, Pixels, RenderPriority, TileDataAddress,
};

use once_cell::sync::Lazy;

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

static BLANK_SCREEN: Lazy<Box<[u8; (GB_WIDTH * 4) * GB_HEIGHT]>> = Lazy::new(|| {
    WHITE
        .repeat(GB_WIDTH * GB_HEIGHT)
        .into_boxed_slice()
        .try_into()
        .unwrap()
});

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
    pub(crate) oam: ObjectAttrTable,
    pub(crate) dma: DirectMemoryAccess,
    scan_dot: Cycle,
    fetch: PixelFetcher,
    fifo: PixelFifo,
    obj_buffer: ObjectBuffer,
    pub(crate) frame_buf: FrameBuffer,
    win_stat: WindowStatus,

    scanline_start: bool,
    to_discard: u8,

    x_pos: u8,
    dot: Cycle,
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
        if !self.ctrl.lcd_enabled() {
            if self.dot > 0 {
                // Check ensures this expensive operation only happens once
                self.frame_buf
                    .get_mut(Device::Guest)
                    .copy_from_slice(BLANK_SCREEN.as_ref());
            }

            self.stat.set_mode(PpuMode::HBlank);
            self.pos.line_y = 0;
            self.dot = 0;
            return;
        }

        self.dot += 1;

        match self.stat.mode() {
            PpuMode::OamScan => {
                // Cycles 1 -> 80

                if self.dot >= 80 {
                    self.x_pos = 0;
                    self.scanline_start = true;
                    self.fetch.back.tile_high_reset = true;
                    self.to_discard = 0;
                    self.fifo.back.clear();
                    self.fifo.obj.clear();

                    // Sort Sprites
                    self.obj_buffer.inner.sort_by(|left, right| {
                        left.zip(*right)
                            .map(|(left, right)| right.x.cmp(&left.x))
                            .unwrap_or(std::cmp::Ordering::Greater)
                    });

                    // if self.obj_buffer.len != 0 {
                    //     dbg!(&self.obj_buffer);
                    // }

                    self.stat.set_mode(PpuMode::Drawing);
                }

                self.scan_oam();
            }
            PpuMode::Drawing => {
                self.draw();

                if self.x_pos == 160 {
                    if self.stat.hblank_int() {
                        // Enable HBlank LCDStat Interrupt
                        self.int.set_lcd_stat(true);
                    }

                    // Done with rendering this frame,
                    // we can reset the ppu x_pos and fetcher state now

                    // Increment Window line counter if scanline had any window pixels on it
                    // only increment once per scanline though
                    if self.win_stat.enabled {
                        self.fetch.back.wl_count += 1;
                    }

                    self.fetch.hblank_reset();
                    self.win_stat.enabled = false;

                    self.obj_buffer.clear();
                    self.stat.set_mode(PpuMode::HBlank);
                }
            }
            PpuMode::HBlank => {
                // This mode will always end at 456 cycles

                if self.dot >= 456 {
                    self.dot %= 456;
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
                        self.fetch.back.wl_count = 0;

                        // Reset WY=LY coincidence flag
                        self.win_stat.coincidence = false;

                        if self.stat.vblank_int() {
                            // Enable Vblank LCDStat Interrupt
                            self.int.set_lcd_stat(true);
                        }

                        // Screen is done drawing
                        self.frame_buf.swap();

                        PpuMode::VBlank
                    } else {
                        if self.stat.oam_int() {
                            // Enable OAM LCDStat Interrupt
                            self.int.set_lcd_stat(true);
                        }

                        self.scan_dot = Default::default();
                        PpuMode::OamScan
                    };

                    self.stat.set_mode(next_mode);
                }
            }
            PpuMode::VBlank => {
                if self.dot >= 456 {
                    self.dot %= 456;
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

                        self.scan_dot = Default::default();
                        self.stat.set_mode(PpuMode::OamScan);
                    }
                }
            }
        }
    }

    fn scan_oam(&mut self) {
        if self.scan_dot % 2 == 0 {
            if self.dma.is_active() {
                return;
            }

            if !self.win_stat.coincidence && self.scan_dot == 0 {
                self.win_stat.coincidence = self.pos.line_y == self.pos.window_y;
            }

            let obj_height = self.ctrl.obj_size().size();
            let attr = self.oam.attribute(self.scan_dot as usize / 2);
            let line_y = self.pos.line_y + 16;

            if attr.x > 0
                && line_y >= attr.y
                && line_y < (attr.y + obj_height)
                && !self.obj_buffer.is_full()
            {
                self.obj_buffer.add(attr);
            }
        }
        self.scan_dot += 1;
    }

    fn draw(&mut self) {
        use FetcherState::*;

        let mut obj_attr = &mut None;

        for maybe_attr in &mut self.obj_buffer.inner {
            match maybe_attr {
                Some(attr) if self.ctrl.obj_enabled() => {
                    if attr.x <= (self.x_pos + 8) {
                        self.fetch.back.reset();
                        self.fetch.back.enabled = false;
                        self.fifo.pause();

                        obj_attr = maybe_attr;
                        break;
                    }
                }
                _ => break,
            }
        }

        if let Some(attr) = obj_attr {
            match self.fetch.obj.state {
                TileNumberA => self.fetch.obj.state = TileNumberB,
                TileNumberB => {
                    self.fetch.obj.tile.with_id(attr.tile_index);
                    self.fetch.obj.state = TileLowA;
                }
                TileLowA => self.fetch.obj.state = TileLowB,
                TileLowB => {
                    let obj_size = self.ctrl.obj_size();

                    let addr = PixelFetcher::obj_addr(attr, &self.pos, obj_size);

                    let byte = self.read_byte(addr);
                    self.fetch.obj.tile.with_low(byte);

                    self.fetch.obj.state = TileHighA;
                }
                TileHighA => self.fetch.obj.state = TileHighB,
                TileHighB => {
                    let obj_size = self.ctrl.obj_size();

                    let addr = PixelFetcher::obj_addr(attr, &self.pos, obj_size);

                    let byte = self.read_byte(addr + 1);
                    self.fetch.obj.tile.with_high(byte);

                    self.fetch.obj.state = ToFifoA;
                }
                ToFifoA => {
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

                    self.fetch.back.enabled = true;
                    self.fifo.resume();
                    let _ = std::mem::take(obj_attr);

                    self.fetch.obj.state = ToFifoB;
                }
                ToFifoB => self.fetch.obj.reset(),
            }
        }

        if self.fetch.back.enabled {
            match self.fetch.back.state {
                TileNumberA => self.fetch.back.state = TileNumberB,
                TileNumberB => {
                    // Are we rendering the Window currently?
                    self.fetch.back.draw_window = self.win_stat.enabled;

                    let addr =
                        self.fetch
                            .back
                            .tile_id_addr(&self.ctrl, &self.pos, self.fetch.x_pos);

                    let id = self.read_byte(addr);
                    self.fetch.back.tile.with_id(id);

                    self.fetch.back.state = TileLowA;
                }
                TileLowA => self.fetch.back.state = TileLowB,
                TileLowB => {
                    let id = self.fetch.back.tile.id.expect("Tile ID present");

                    let addr = self.fetch.back.tile_addr(&self.ctrl, &self.pos, id);
                    let byte = self.read_byte(addr);
                    self.fetch.back.tile.with_low(byte);

                    self.fetch.back.state = TileHighA;
                }
                TileHighA => self.fetch.back.state = TileHighB,
                TileHighB => {
                    let id = self.fetch.back.tile.id.expect("Tile ID present");

                    let addr = self.fetch.back.tile_addr(&self.ctrl, &self.pos, id);
                    let byte = self.read_byte(addr + 1);
                    self.fetch.back.tile.with_high(byte);

                    if self.fetch.back.tile_high_reset {
                        self.fetch.back.reset();
                        self.fetch.back.tile_high_reset = false;
                    } else {
                        self.fetch.back.state = ToFifoA;
                    }
                }
                ToFifoA => {
                    if self.fetch.send_to_fifo(&mut self.fifo).is_ok() {
                        self.fetch.x_pos += 1;
                        self.fetch.back.state = ToFifoB;
                    }
                }
                ToFifoB => self.fetch.back.reset(),
            }
        }

        if self.fifo.is_enabled() {
            if self.x_pos == 0 && self.scanline_start {
                self.to_discard = self.pos.scroll_x % 8;
                self.scanline_start = false;
            }

            if !self.win_stat.enabled && self.to_discard > 0 && !self.fifo.back.is_empty() {
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

                self.frame_buf.get_mut(Device::Guest)[i..(i + rgba.len())].copy_from_slice(&rgba);
                self.x_pos += 1;
            }

            if self.ctrl.window_enabled()
                && !self.win_stat.enabled
                && self.win_stat.coincidence
                && self.x_pos as i16 >= self.pos.window_x as i16 - 7
            {
                self.win_stat.enabled = true;
                self.fetch.back.reset();
                self.fetch.x_pos = 0;
                self.fifo.back.clear();
            }
        }
    }

    fn clock_fifo(&mut self) -> Option<GrayShade> {
        use RenderPriority::*;

        self.fifo
            .back
            .pop_front()
            .map(|bg| match self.fifo.obj.pop_front() {
                Some(obj) => match obj.priority {
                    _ if obj.shade_id == 0 => self.bg_pixel(bg),
                    BackgroundAndWindow if bg.shade_id != 0 => self.bg_pixel(bg),
                    _ => self.obj_pixel(obj),
                },
                None => self.bg_pixel(bg),
            })
    }

    fn obj_pixel(&self, obj: ObjPixelProperty) -> GrayShade {
        use ObjectPaletteKind::*;
        assert_ne!(obj.shade_id, 0);

        let p0 = &self.monochrome.obj_palette_0;
        let p1 = &self.monochrome.obj_palette_1;

        match obj.palette_kind {
            Zero => p0.shade(obj.shade_id).expect("Object shade id is non-zero"),
            One => p1.shade(obj.shade_id).expect("Object shade id is non-zero"),
        }
    }

    fn bg_pixel(&self, bg: BgPixelProperty) -> GrayShade {
        let bg_palette = &self.monochrome.bg_palette;

        if self.ctrl.bg_win_enabled() {
            bg_palette.shade(bg.shade_id)
        } else {
            bg_palette.shade(0)
        }
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            vram: Box::new([0u8; VRAM_SIZE]),
            dot: Default::default(),
            frame_buf: FrameBuffer::new().expect("create frame buffers"),
            int: Default::default(),
            ctrl: LCDControl(0),
            monochrome: Default::default(),
            pos: Default::default(),
            stat: LCDStatus(0x80), // bit 7 is always 1
            oam: Default::default(),
            scan_dot: Default::default(),
            fetch: Default::default(),
            fifo: Default::default(),
            obj_buffer: Default::default(),
            win_stat: Default::default(),
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

#[derive(Debug)]
pub(crate) struct Monochrome {
    /// 0xFF47 | BGP - Background Palette Data
    pub(crate) bg_palette: BackgroundPalette,
    /// 0xFF48 | OBP0 - Object Palette 0 Data
    pub(crate) obj_palette_0: ObjectPalette,
    /// 0xFF49 | OBP1 - Object Palette 1 Data
    pub(crate) obj_palette_1: ObjectPalette,
}

impl Default for Monochrome {
    fn default() -> Self {
        Self {
            bg_palette: BackgroundPalette(0),
            obj_palette_0: ObjectPalette(0),
            obj_palette_1: ObjectPalette(0),
        }
    }
}

#[derive(Debug)]
pub(crate) struct ObjectAttrTable {
    buf: Box<[u8; OAM_SIZE]>,
}

impl BusIo for ObjectAttrTable {
    fn read_byte(&self, addr: u16) -> u8 {
        let index = (addr - 0xFE00) as usize;
        self.buf[index]
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        let index = (addr - 0xFE00) as usize;
        self.buf[index] = byte;
    }
}

impl ObjectAttrTable {
    fn attribute(&self, index: usize) -> ObjectAttr {
        let start = index * 4;

        let slice: &[u8; 4] = self.buf[start..(start + 4)]
            .try_into()
            .expect("TryInto trait called on a &[u8; 4]");

        slice.into()
    }
}

impl Default for ObjectAttrTable {
    fn default() -> Self {
        Self {
            buf: Box::new([0; OAM_SIZE]),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ObjectAttr {
    y: u8,
    x: u8,
    tile_index: u8,
    flags: ObjectFlags,
}

impl From<[u8; 4]> for ObjectAttr {
    fn from(bytes: [u8; 4]) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_index: bytes[2],
            flags: bytes[3].into(),
        }
    }
}

impl<'a> From<&'a [u8; 4]> for ObjectAttr {
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
    inner: [Option<ObjectAttr>; OBJECT_LIMIT],
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

    fn add(&mut self, attr: ObjectAttr) {
        self.inner[self.len] = Some(attr);
        self.len += 1;
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

    fn obj_addr(attr: &ObjectAttr, pos: &ScreenPosition, size: ObjectSize) -> u16 {
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
    fn reset(&mut self);
    fn hblank_reset(&mut self);
}

#[derive(Debug)]
struct BackgroundFetcher {
    state: FetcherState,
    tile: TileBuilder,
    wl_count: u8,
    draw_window: bool,
    enabled: bool,
    tile_high_reset: bool,
}

impl BackgroundFetcher {
    fn tile_id_addr(&self, control: &LCDControl, pos: &ScreenPosition, x_pos: u8) -> u16 {
        let line_y = pos.line_y;
        let scroll_y = pos.scroll_y;
        let scroll_x = pos.scroll_x;
        let is_window = self.draw_window;

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
            self.wl_count as u16 / 8
        } else {
            ((line_y as u16 + scroll_y as u16) & 0xFF) / 8
        };

        let x_offset = (scx_offset + x_pos) & 0x1F;
        let offset = (32 * y_offset) + (x_offset as u16);

        tile_map_addr + (offset & 0x3FF)
    }

    fn tile_addr(&mut self, control: &LCDControl, pos: &ScreenPosition, id: u8) -> u16 {
        let line_y = pos.line_y;
        let scroll_y = pos.scroll_y;

        let tile_data_addr = match control.tile_data_addr() {
            TileDataAddress::X8800 => 0x9000u16.wrapping_add((id as i8 as i16 * 16) as u16),
            TileDataAddress::X8000 => 0x8000 + (id as u16 * 16),
        };

        let offset = if self.draw_window {
            self.wl_count as u16 % 8
        } else {
            (line_y as u16 + scroll_y as u16) % 8
        };

        tile_data_addr + (offset * 2)
    }
}

impl Fetcher for BackgroundFetcher {
    fn reset(&mut self) {
        self.state = Default::default();
        self.tile = Default::default();
    }

    fn hblank_reset(&mut self) {
        self.reset();

        self.draw_window = false;

        self.enabled = true;
    }
}

impl Default for BackgroundFetcher {
    fn default() -> Self {
        Self {
            state: Default::default(),
            tile: Default::default(),
            draw_window: Default::default(),
            wl_count: Default::default(),
            enabled: true,
            tile_high_reset: true,
        }
    }
}

#[derive(Debug, Default)]
struct ObjectFetcher {
    state: FetcherState,
    tile: TileBuilder,
}

impl Fetcher for ObjectFetcher {
    fn reset(&mut self) {
        self.state = Default::default();
        self.tile = Default::default();
    }

    fn hblank_reset(&mut self) {
        self.reset()
    }
}

#[derive(Debug, Clone, Copy)]
enum FetcherState {
    TileNumberA,
    TileNumberB,
    TileLowA,
    TileLowB,
    TileHighA,
    TileHighB,
    ToFifoA,
    ToFifoB,
}

impl Default for FetcherState {
    fn default() -> Self {
        Self::TileNumberA
    }
}

#[derive(Debug, Default)]
struct BgPixelProperty {
    shade_id: u8,
}

#[derive(Debug)]
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

    fn with_low(&mut self, data: u8) {
        self.low = Some(data);
    }

    fn with_high(&mut self, data: u8) {
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
    enabled: bool,
}

pub(crate) mod dbg {
    use super::{Ppu, PpuMode};
    use crate::Cycle;

    pub(crate) fn ly(ppu: &Ppu) -> u8 {
        ppu.pos.line_y
    }

    pub(crate) fn scx(ppu: &Ppu) -> u8 {
        ppu.pos.scroll_x
    }

    pub(crate) fn scy(ppu: &Ppu) -> u8 {
        ppu.pos.scroll_y
    }

    pub(crate) fn mode(ppu: &Ppu) -> PpuMode {
        ppu.stat.mode()
    }

    pub(crate) fn wx(ppu: &Ppu) -> i16 {
        ppu.pos.window_x as i16
    }

    pub(crate) fn wy(ppu: &Ppu) -> i16 {
        ppu.pos.window_y as i16
    }

    pub(crate) fn dot(ppu: &Ppu) -> Cycle {
        ppu.dot
    }
}

#[derive(Debug)]
pub struct FrameBuffer {
    buf: [Box<[u8; Self::FRAME_LEN]>; 2],
    current: bool,
}

#[derive(PartialEq)]
pub enum Device {
    Guest,
    Host,
}

impl FrameBuffer {
    const FRAME_LEN: usize = GB_WIDTH * std::mem::size_of::<u32>() * GB_HEIGHT;

    pub fn new() -> Result<Self, FrameBufferError> {
        Ok(Self {
            buf: [
                vec![0; Self::FRAME_LEN]
                    .into_boxed_slice()
                    .try_into()
                    .map_err(|_| FrameBufferError::TryFrom)?,
                vec![0; Self::FRAME_LEN]
                    .into_boxed_slice()
                    .try_into()
                    .map_err(|_| FrameBufferError::TryFrom)?,
            ],
            current: false,
        })
    }

    pub fn swap(&mut self) {
        self.current = !self.current;
    }

    pub fn get_mut(&mut self, device: Device) -> &mut [u8; Self::FRAME_LEN] {
        let idx = match device {
            Device::Guest => self.current,
            Device::Host => !self.current,
        };

        &mut *self.buf[idx as usize]
    }

    pub fn get(&self, device: Device) -> &[u8; Self::FRAME_LEN] {
        let idx = match device {
            Device::Guest => self.current,
            Device::Host => !self.current,
        };

        &*self.buf[idx as usize]
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FrameBufferError {
    #[error("Failed to coerce boxed slice to boxed array")]
    TryFrom,
}
