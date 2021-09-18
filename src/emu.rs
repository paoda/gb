use crate::apu::gen::SampleProducer;
use crate::cpu::Cpu;
use crate::joypad::{self, Joypad};
use crate::{Cycle, GB_HEIGHT, GB_WIDTH};
use gilrs::Gilrs;
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
        self.cpu.step()
    }

    fn load_cart(&mut self, rom: Vec<u8>) {
        self.cpu.bus_mut().load_cart(rom)
    }

    fn joyp_mut(&mut self) -> &mut Joypad {
        &mut self.cpu.bus_mut().joypad
    }

    pub fn set_prod(&mut self, prod: SampleProducer<f32>) {
        self.cpu.bus_mut().apu.attach_producer(prod)
    }

    pub fn title(&self) -> &str {
        self.cpu.bus().cart_title().unwrap_or(DEFAULT_TITLE)
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
