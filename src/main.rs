use anyhow::{anyhow, Result};
use gb::{Cycles, LR35902};
use pixels::{Pixels, SurfaceTexture};
use std::env::args;
use std::time::{Duration, Instant};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};
use winit_input_helper::WinitInputHelper;

// 160 x 144
const GB_WIDTH: u32 = 160;
const GB_HEIGHT: u32 = 144;
const SCALE: f64 = 5.0;

const LR35902_CLOCK_SPEED: u32 = 4194304; // Hz | 4.194304Mhz
const LR35902_CYCLE_TIME: f64 = 1.0f64 / LR35902_CLOCK_SPEED as f64;
const CYCLES_IN_FRAME: Cycles = Cycles::new(70224);

fn main() -> Result<()> {
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = create_window(&event_loop)?;
    let mut pixels = create_pixels(&window)?;

    let mut game_boy = match args().nth(1) {
        Some(boot_path) => LR35902::boot_new(&boot_path),
        None => LR35902::new(),
    };

    game_boy.load_cartridge("bin/cpu_instrs.gb");

    let mut now = Instant::now();
    let mut cycles_in_frame = Cycles::default();
    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            if pixels
                .render()
                .map_err(|e| anyhow!("pixels.render() failed: {}", e))
                .is_err()
            {
                *control_flow = ControlFlow::Exit;
                return;
            }
        }

        if input.update(&event) {
            if input.key_pressed(VirtualKeyCode::Escape) || input.quit() {
                *control_flow = ControlFlow::Exit;
                return;
            }

            if let Some(size) = input.window_resized() {
                pixels.resize(size.width, size.height);
            }

            // Emulate Game Boy
            let delta = now.elapsed().subsec_nanos();
            now = Instant::now();

            let cycle_time = Duration::from_secs_f64(LR35902_CYCLE_TIME).subsec_nanos();
            let pending_cycles = Cycles::new(delta / cycle_time);

            let mut elapsed_cycles = Cycles::default();
            while elapsed_cycles <= pending_cycles {
                elapsed_cycles += game_boy.step();
            }

            cycles_in_frame += elapsed_cycles;

            if cycles_in_frame >= CYCLES_IN_FRAME {
                let ppu = game_boy.get_ppu();
                let frame = pixels.get_frame();
                ppu.copy_to_gui(frame);
                window.request_redraw();

                cycles_in_frame = Cycles::default()
            }
        }
    });
}

pub fn create_window(event_loop: &EventLoop<()>) -> Result<Window> {
    let size = LogicalSize::new((GB_WIDTH as f64) * SCALE, (GB_HEIGHT as f64) * SCALE);
    Ok(WindowBuilder::new()
        .with_title("DMG-1 Emulator")
        .with_inner_size(size)
        .with_min_inner_size(size)
        .build(&event_loop)?)
}

pub fn create_pixels(window: &Window) -> Result<Pixels<Window>> {
    let window_size = window.inner_size();
    let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, window);
    Ok(Pixels::new(GB_WIDTH, GB_HEIGHT, surface_texture)?)
}
