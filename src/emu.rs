use crate::cpu::Cpu as SM83;
use crate::instruction::Cycle;
use crate::joypad;
use crate::ppu::Ppu;
use anyhow::Result;
use gilrs::Gilrs;
use std::time::Duration;
use winit_input_helper::WinitInputHelper;

pub const SM83_CYCLE_TIME: Duration = Duration::from_nanos(1_000_000_000 / SM83_CLOCK_SPEED);
pub const CYCLES_IN_FRAME: Cycle = Cycle::new(456 * 154); // 456 Cycles times 154 scanlines
pub(crate) const SM83_CLOCK_SPEED: u64 = 0x40_0000; // Hz which is 4.194304Mhz
const DEFAULT_TITLE: &str = "DMG-01 Emulator";

pub fn init(boot_path: Option<&str>, rom_path: &str) -> Result<SM83> {
    let mut cpu = match boot_path {
        Some(path) => SM83::boot_new(path)?,
        None => SM83::new(),
    };

    eprintln!("Initialized GB Emulator");

    cpu.load_cartridge(rom_path)?;
    Ok(cpu)
}

pub fn rom_title(game_boy: &SM83) -> &str {
    game_boy.rom_title().unwrap_or(DEFAULT_TITLE)
}

pub fn run(
    game_boy: &mut SM83,
    gamepad: &mut Gilrs,
    input: &WinitInputHelper,
    pending: Cycle,
) -> Cycle {
    let mut elapsed = Cycle::new(0);

    while elapsed < pending {
        elapsed += run_unsynced(game_boy, gamepad, input);
    }

    elapsed
}

pub fn run_unsynced(game_boy: &mut SM83, gamepad: &mut Gilrs, input: &WinitInputHelper) -> Cycle {
    if let Some(event) = gamepad.next_event() {
        joypad::handle_gamepad_input(game_boy.joypad_mut(), event);
    } else {
        joypad::handle_keyboard_input(game_boy.joypad_mut(), input);
    }

    game_boy.step()
}

pub fn draw(ppu: &Ppu, frame: &mut [u8]) {
    ppu.copy_to_gui(frame);
}
