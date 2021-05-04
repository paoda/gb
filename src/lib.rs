pub use cpu::Cpu as LR35902;
pub use instruction::Cycle;
pub use joypad::ButtonState;

pub const GB_WIDTH: usize = 160;
pub const GB_HEIGHT: usize = 144;
pub const LR35902_CLOCK_SPEED: u32 = 0x400000; // Hz | 4.194304Mhz

mod bus;
mod cartridge;
mod cpu;
mod high_ram;
mod instruction;
mod interrupt;
mod joypad;
mod ppu;
mod serial;
mod sound;
mod timer;
mod work_ram;
