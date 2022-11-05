use crate::apu::gen::SampleProducer;
use crate::bus::BOOT_SIZE;
use crate::cartridge::Cartridge;
use crate::cpu::Cpu;
use crate::{Cycle, GB_HEIGHT, GB_WIDTH};
use clap::crate_name;
use gilrs::Gilrs;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;
use thiserror::Error;
use winit::event::KeyboardInput;
use winit::event_loop::ControlFlow;

pub const SM83_CYCLE_TIME: Duration = Duration::from_nanos(1_000_000_000 / SM83_CLOCK_SPEED);
pub const CYCLES_IN_FRAME: Cycle = 456 * 154; // 456 Cycles times 154 scanlines
pub(crate) const SM83_CLOCK_SPEED: u64 = 0x40_0000; // Hz which is 4.194304Mhz
const DEFAULT_TITLE: &str = "Game Boy Screen";

#[inline]
pub fn run_frame(cpu: &mut Cpu, gamepad: &mut Gilrs, key: KeyboardInput) {
    run(cpu, gamepad, key, CYCLES_IN_FRAME)
}

#[inline]
pub fn run(cpu: &mut Cpu, gamepad: &mut Gilrs, key: KeyboardInput, cycles: Cycle) {
    let mut elapsed = 0;

    if let Some(event) = gamepad.next_event() {
        crate::joypad::handle_gamepad_input(&mut cpu.bus.joyp, event);
    }
    crate::joypad::handle_keyboard_input(&mut cpu.bus.joyp, key);

    while elapsed < cycles {
        elapsed += cpu.step();
    }
}

pub fn save_and_exit(cpu: &Cpu, control_flow: &mut ControlFlow) {
    write_save(cpu);
    *control_flow = ControlFlow::Exit;
}

#[inline]
pub fn pixel_buf(cpu: &Cpu) -> &[u8; GB_HEIGHT * GB_WIDTH * 4] {
    use crate::ppu::Device;
    cpu.bus.ppu.frame_buf.get(Device::Host)
}

pub fn from_boot_rom<P: AsRef<Path>>(path: P) -> std::io::Result<Cpu> {
    Ok(Cpu::new(read_boot(path)?))
}

pub fn read_game_rom<P: AsRef<Path>>(cpu: &mut Cpu, path: P) -> std::io::Result<()> {
    cpu.bus.cart = Some(Cartridge::new(std::fs::read(path.as_ref())?));
    Ok(())
}

pub fn set_audio_prod(cpu: &mut Cpu, prod: SampleProducer<f32>) {
    cpu.bus.apu.prod = Some(prod);
}

pub fn rom_title(cpu: &Cpu) -> &str {
    cpu.bus
        .cart
        .as_ref()
        .and_then(|c| c.title.as_deref())
        .unwrap_or(DEFAULT_TITLE)
}

pub fn write_save(cpu: &Cpu) {
    match cpu.bus.cart.as_ref() {
        Some(cart) => match write_save_to_file(cart) {
            Ok(path) => tracing::info!("Wrote to save at {:?}", path),
            Err(err @ SaveError::NotApplicable) => tracing::warn!("Unable to Save: {:?}", err),
            Err(SaveError::DiffSize) => unreachable!(),
            Err(SaveError::Io(err)) => tracing::error!("{:?}", err),
        },
        None => tracing::error!("No cartridge is currently present"),
    }
}

pub fn load_save(cpu: &mut Cpu) {
    match cpu.bus.cart.as_mut() {
        Some(cart) => match read_save_from_file(cart) {
            Ok(path) => tracing::info!("Loaded save from {:?}", path),
            Err(err @ SaveError::NotApplicable) => tracing::warn!("Unable to load save: {}", err),
            Err(err @ SaveError::DiffSize) => tracing::error!("Unable to load save: {}", err),
            Err(SaveError::Io(err)) => match err.kind() {
                std::io::ErrorKind::NotFound => tracing::warn!("Save not found"),
                _ => tracing::error!("{:?}", err),
            },
        },
        None => tracing::error!("No cartridge is currently present"),
    }
}

fn write_save_to_file(cart: &Cartridge) -> Result<PathBuf, SaveError> {
    match cart.title.as_ref().zip(cart.ext_ram()) {
        Some((title, ram)) => {
            let mut save_path = data_path().unwrap_or_else(|| PathBuf::from("."));
            save_path.push(title);
            save_path.set_extension("sav");

            let mut file = File::create(&save_path)?;
            file.write_all(ram)?;
            Ok(save_path)
        }
        None => Err(SaveError::NotApplicable),
    }
}

fn read_save_from_file(cart: &mut Cartridge) -> Result<PathBuf, SaveError> {
    match cart.title.clone().zip(cart.ext_ram_mut()) {
        Some((title, ext_ram)) => {
            let mut save_path = data_path().unwrap_or_else(|| PathBuf::from("."));
            save_path.push(title);
            save_path.set_extension("sav");

            let mut file = File::open(&save_path)?;
            let mut memory = Vec::new();
            file.read_to_end(&mut memory)?;

            if ext_ram.len() != memory.len() {
                return Err(SaveError::DiffSize);
            }

            ext_ram.copy_from_slice(&memory);
            Ok(save_path)
        }
        None => Err(SaveError::NotApplicable),
    }
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

#[derive(Debug, Error)]
pub enum SaveError {
    #[error("cartridge lacks title and/or external ram")]
    NotApplicable,
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("save file size differs from external ram")]
    DiffSize,
}
