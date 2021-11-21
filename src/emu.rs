use crate::apu::gen::SampleProducer;
use crate::bus::BOOT_SIZE;
use crate::cpu::Cpu;
use crate::{Cycle, GB_HEIGHT, GB_WIDTH};
use clap::crate_name;
use gilrs::Gilrs;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;
use winit::event::KeyboardInput;

pub const SM83_CYCLE_TIME: Duration = Duration::from_nanos(1_000_000_000 / SM83_CLOCK_SPEED);
pub const CYCLES_IN_FRAME: Cycle = 456 * 154; // 456 Cycles times 154 scanlines
pub(crate) const SM83_CLOCK_SPEED: u64 = 0x40_0000; // Hz which is 4.194304Mhz
const DEFAULT_TITLE: &str = "DMG-01 Emulator";

pub fn run_frame(cpu: &mut Cpu, gamepad: &mut Gilrs, key: KeyboardInput) -> Cycle {
    let mut elapsed = 0;

    if let Some(event) = gamepad.next_event() {
        crate::joypad::handle_gamepad_input(&mut cpu.bus.joyp, event);
    }
    crate::joypad::handle_keyboard_input(&mut cpu.bus.joyp, key);

    while elapsed < CYCLES_IN_FRAME {
        elapsed += cpu.step();
    }

    elapsed
}

pub fn pixel_buf(cpu: &Cpu) -> &[u8; GB_HEIGHT * GB_WIDTH * 4] {
    cpu.bus.ppu.frame_buf.as_ref()
}

pub fn from_boot_rom<P: AsRef<Path>>(path: P) -> std::io::Result<Cpu> {
    Ok(Cpu::new(read_boot(path)?))
}

pub fn read_game_rom<P: AsRef<Path>>(cpu: &mut Cpu, path: P) -> std::io::Result<()> {
    Ok(cpu.bus.load_cart(std::fs::read(path.as_ref())?))
}

pub fn set_audio_producer(cpu: &mut Cpu, producer: SampleProducer<f32>) {
    cpu.bus.apu.attach_producer(producer);
}

pub fn rom_title(cpu: &Cpu) -> &str {
    cpu.bus.cart_title().unwrap_or(DEFAULT_TITLE)
}

pub fn write_save(cpu: &Cpu) -> std::io::Result<()> {
    if let Some(ext_ram) = cpu.bus.cart.as_ref().map(|c| c.ext_ram()).flatten() {
        if let Some(title) = cpu.bus.cart_title() {
            let mut save_path = data_path().unwrap_or_else(|| PathBuf::from("."));
            save_path.push(title);
            save_path.set_extension("sav");

            let mut file = File::create(save_path)?;
            file.write_all(ext_ram)?;
        }
    }

    Ok(())
}

pub fn load_save(cpu: &mut Cpu) -> std::io::Result<()> {
    if let Some(cart) = &mut cpu.bus.cart {
        if let Some(title) = cart.title() {
            let mut save_path = data_path().unwrap_or_else(|| PathBuf::from("."));
            save_path.push(title);
            save_path.set_extension("sav");

            if let Ok(mut file) = File::open(&save_path) {
                tracing::info!("Load {:?}", save_path);

                let mut memory = Vec::new();
                file.read_to_end(&mut memory)?;
                cart.write_ext_ram(memory);
            }
        }
    }

    Ok(())
}

fn read_boot<P: AsRef<Path>>(path: P) -> std::io::Result<[u8; BOOT_SIZE]> {
    let mut buf = [0; BOOT_SIZE];
    let mut file = File::open(path.as_ref())?;

    file.read_exact(&mut buf)?;
    Ok(buf)
}

fn data_path() -> Option<PathBuf> {
    match directories_next::ProjectDirs::from("dev", "musuka", crate_name!()) {
        Some(dirs) => {
            let data_local = dirs.data_local_dir();
            std::fs::create_dir_all(data_local).ok()?;
            Some(data_local.to_path_buf())
        }
        None => None,
    }
}
