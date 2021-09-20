use crate::apu::gen::SampleProducer;
use crate::cpu::Cpu;
use crate::joypad::{self, Joypad};
use crate::{Cycle, GB_HEIGHT, GB_WIDTH};
use clap::crate_name;
use gilrs::Gilrs;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Duration;
use winit_input_helper::WinitInputHelper;

pub const SM83_CYCLE_TIME: Duration = Duration::from_nanos(1_000_000_000 / SM83_CLOCK_SPEED);
pub const CYCLES_IN_FRAME: Cycle = 456 * 154; // 456 Cycles times 154 scanlines
pub(crate) const SM83_CLOCK_SPEED: u64 = 0x40_0000; // Hz which is 4.194304Mhz
const DEFAULT_TITLE: &str = "DMG-01 Emulator";

pub fn run_frame(emu: &mut Emulator, gamepad: &mut Gilrs, key: &WinitInputHelper) -> Cycle {
    let mut elapsed = 0;

    if let Some(event) = gamepad.next_event() {
        joypad::handle_gamepad_input(emu.joyp_mut(), event);
    }
    joypad::handle_keyboard_input(emu.joyp_mut(), key);

    while elapsed < CYCLES_IN_FRAME {
        elapsed += emu.step();
    }

    elapsed
}

pub fn draw_frame(emu: &Emulator, buf: &mut [u8; GB_HEIGHT * GB_WIDTH * 4]) {
    buf.copy_from_slice(emu.cpu.bus().ppu.frame_buf());
}

pub struct Emulator {
    cpu: Cpu,
    timestamp: Cycle,
}

impl Emulator {
    fn new(cpu: Cpu) -> Self {
        Self {
            cpu,
            timestamp: Default::default(),
        }
    }

    fn step(&mut self) -> Cycle {
        let cycles = self.cpu.step();
        self.timestamp += cycles;
        cycles
    }

    fn load_cart(&mut self, rom: Vec<u8>) {
        self.cpu.bus_mut().load_cart(rom);
    }

    #[inline]
    fn joyp_mut(&mut self) -> &mut Joypad {
        self.cpu.bus_mut().joyp_mut()
    }

    pub fn set_prod(&mut self, prod: SampleProducer<f32>) {
        self.cpu.bus_mut().apu.attach_producer(prod)
    }

    pub fn title(&self) -> &str {
        self.cpu.bus().cart_title().unwrap_or(DEFAULT_TITLE)
    }

    pub fn try_write_sav(&self) -> std::io::Result<()> {
        if let Some(ext_ram) = self.cpu.bus().cart().map(|c| c.ext_ram()).flatten() {
            if let Some(title) = self.cpu.bus().cart_title() {
                let mut save_path = Self::data_path().unwrap_or_else(|| PathBuf::from("."));
                save_path.push(title);
                save_path.set_extension("sav");

                let mut file = File::create(save_path)?;
                file.write_all(ext_ram)?;
            }
        }

        Ok(())
    }

    pub fn try_load_sav(&mut self) -> std::io::Result<()> {
        if let Some(cart) = self.cpu.bus_mut().cart_mut() {
            if let Some(title) = cart.title() {
                let mut save_path = Self::data_path().unwrap_or_else(|| PathBuf::from("."));
                save_path.push(title);
                save_path.set_extension("sav");

                if let Ok(mut file) = File::open(save_path) {
                    let mut memory = Vec::new();
                    file.read_to_end(&mut memory)?;

                    cart.write_ext_ram(memory);
                }
            }
        }

        Ok(())
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
}

pub mod build {
    use std::fs::File;
    use std::io::{Read, Result};
    use std::path::Path;

    use crate::bus::BOOT_SIZE;
    use crate::cpu::Cpu;

    use super::Emulator;

    #[derive(Debug, Default)]
    pub struct EmulatorBuilder {
        boot: Option<[u8; BOOT_SIZE]>,
        cart: Option<Vec<u8>>,
    }

    impl EmulatorBuilder {
        pub fn new() -> Self {
            Default::default()
        }

        pub fn with_boot<P: AsRef<Path>>(mut self, path: P) -> Result<Self> {
            let mut file = File::open(path.as_ref())?;

            let mut buf = [0x00; BOOT_SIZE];
            file.read_exact(&mut buf)?;

            self.boot = Some(buf);
            Ok(self)
        }

        pub fn with_cart<P: AsRef<Path>>(mut self, path: P) -> Result<Self> {
            let mut file = File::open(path.as_ref())?;

            let mut buf = Vec::new();
            file.read_to_end(&mut buf)?;

            self.cart = Some(buf);
            Ok(self)
        }

        pub fn finish(mut self) -> Emulator {
            let mut emu = Emulator::new(match self.boot {
                Some(rom) => Cpu::with_boot(rom),
                None => Cpu::without_boot(),
            });

            if let Some(rom) = self.cart.take() {
                emu.load_cart(rom)
            }

            emu
        }
    }
}
