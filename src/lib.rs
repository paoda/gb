pub use apu::gen::init as spsc_init;
pub type Cycle = u64;

pub const GB_WIDTH: usize = 160;
pub const GB_HEIGHT: usize = 144;

mod apu;
mod bus;
mod cartridge;
mod cpu;
pub mod emu;
mod high_ram;
mod instruction;
mod interrupt;
mod joypad;
mod ppu;
mod serial;
mod timer;
mod work_ram;
