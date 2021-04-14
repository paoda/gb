use anyhow::{anyhow, Result};
use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use gb::LR35902_CLOCK_SPEED;
use gb::{Cycle, LR35902};
use pixels::{Pixels, SurfaceTexture};
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

const LR35902_CYCLE_TIME: f64 = 1.0f64 / LR35902_CLOCK_SPEED as f64;
const CYCLES_IN_FRAME: Cycle = Cycle::new(70224);

fn main() -> Result<()> {
    let app = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!());

    let m = app
        .arg(
            Arg::with_name("rom")
                .value_name("ROM_FILE")
                .takes_value(true)
                .required(true)
                .index(1)
                .help("Path to the Game ROM"),
        )
        .arg(
            Arg::with_name("boot")
                .short("b")
                .long("boot")
                .value_name("FILE")
                .takes_value(true)
                .help("Path to Boot ROM"),
        )
        .get_matches();

    let mut game_boy = match m.value_of("boot") {
        Some(path) => LR35902::boot_new(path).expect("Failed to load boot ROM"),
        None => LR35902::new(),
    };

    // This is a required value so if we program gets here,
    // a string **will** have been provided to the rom argument
    let rom_path = m
        .value_of("rom")
        .expect("ROM Path not provided despite it being a required argument");

    game_boy
        .load_cartridge(rom_path)
        .expect("Failed to load ROM");

    let default_title = "DMG-01 Emulator";
    let cartridge_title = game_boy.rom_title().unwrap_or(&default_title);

    // Initialize GUI
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = create_window(&event_loop, cartridge_title)?;
    let mut pixels = create_pixels(&window)?;

    let mut now = Instant::now();
    let mut cycles_in_frame: Cycle = Default::default();
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
            let pending_cycles = Cycle::new(delta / cycle_time);

            let mut elapsed_cycles: Cycle = Default::default();
            while elapsed_cycles <= pending_cycles {
                elapsed_cycles += game_boy.step();
            }

            cycles_in_frame += elapsed_cycles;

            if cycles_in_frame >= CYCLES_IN_FRAME {
                let ppu = game_boy.get_ppu();
                let frame = pixels.get_frame();
                ppu.copy_to_gui(frame);
                window.request_redraw();

                cycles_in_frame = Default::default()
            }
        }
    });
}

fn create_window(event_loop: &EventLoop<()>, title: &str) -> Result<Window> {
    let size = LogicalSize::new((GB_WIDTH as f64) * SCALE, (GB_HEIGHT as f64) * SCALE);
    Ok(WindowBuilder::new()
        .with_title(title)
        .with_inner_size(size)
        .with_min_inner_size(size)
        .build(&event_loop)?)
}

fn create_pixels(window: &Window) -> Result<Pixels<Window>> {
    let window_size = window.inner_size();
    let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, window);
    Ok(Pixels::new(GB_WIDTH, GB_HEIGHT, surface_texture)?)
}
