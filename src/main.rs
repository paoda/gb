use anyhow::{anyhow, Result};
use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use gb::Egui;
use gb::LR35902_CLOCK_SPEED;
use gb::{handle_gamepad_input, Cycle, LR35902};
use gilrs::Gilrs;
use pixels::{Pixels, SurfaceTexture};
use std::time::{Duration, Instant};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};
use winit_input_helper::WinitInputHelper;

#[cfg(feature = "debug")]
use gb::RegisterPair;

// 160 x 144
const GB_WIDTH: u32 = 160;
const GB_HEIGHT: u32 = 144;
const SCALE: f64 = 5.0;

const LR35902_CYCLE_TIME: f64 = 1.0f64 / LR35902_CLOCK_SPEED as f64;
const CYCLES_IN_FRAME: Cycle = Cycle::new(70224);

#[cfg(feature = "debug")]
const STEP_MODE_BY_DEFAULT: bool = true;

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
    let cartridge_title = game_boy.rom_title().unwrap_or(default_title);

    // Initialize Gamepad Support
    let mut gilrs = Gilrs::new().expect("Failed to initialize Gilrs");

    // Initialize GUI
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = create_window(&event_loop, cartridge_title)?;

    let (mut pixels, mut egui) = {
        let size = window.inner_size();
        let scale_factor = window.scale_factor();
        let surface_texture = SurfaceTexture::new(size.width, size.height, &window);
        let pixels = Pixels::new(GB_WIDTH, GB_HEIGHT, surface_texture)?;
        let egui = Egui::new(size.width, size.height, scale_factor, pixels.context());

        (pixels, egui)
    };

    let mut now = Instant::now();
    let mut cycles_in_frame: Cycle = Default::default();

    #[cfg(feature = "debug")]
    let mut step_mode = STEP_MODE_BY_DEFAULT;

    event_loop.run(move |event, _, control_flow| {
        // Update egui
        egui.handle_event(&event);

        if let Event::RedrawRequested(_) = event {
            // Prepare egui
            egui.prepare(&game_boy);

            // Render everything together
            let render_result = pixels.render_with(|encoder, target, ctx| {
                // Render the texture
                ctx.scaling_renderer.render(encoder, target);

                // Render egui
                egui.render(encoder, target, ctx);
            });

            if render_result
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

            #[cfg(feature = "debug")]
            if input.key_pressed(VirtualKeyCode::S) {
                step_mode = !step_mode;
            }

            if let Some(scale_factor) = input.scale_factor() {
                egui.scale_factor(scale_factor);
            }

            if let Some(size) = input.window_resized() {
                pixels.resize_surface(size.width, size.height);
                egui.resize(size.width, size.height);
            }

            // Emulate Game Boy
            let mut elapsed_cycles: Cycle = Default::default();
            let delta = now.elapsed().subsec_nanos();
            now = Instant::now();

            #[cfg(feature = "debug")]
            if step_mode {
                if input.key_pressed(VirtualKeyCode::Space) {
                    if let Some(event) = gilrs.next_event() {
                        handle_gamepad_input(&mut game_boy, event);
                    }

                    for _ in 0..egui.config.spacebar_step {
                        elapsed_cycles += game_boy.step();
                    }

                    cycles_in_frame %= CYCLES_IN_FRAME;
                }

                game_boy.get_ppu().copy_to_gui(pixels.get_frame());
                window.request_redraw();
                return;
            }

            let cycle_time = Duration::from_secs_f64(LR35902_CYCLE_TIME).subsec_nanos();
            let pending_cycles = Cycle::new(delta / cycle_time);

            while elapsed_cycles <= pending_cycles {
                if let Some(event) = gilrs.next_event() {
                    handle_gamepad_input(&mut game_boy.bus.joypad, event);
                }

                elapsed_cycles += game_boy.step();

                #[cfg(feature = "debug")]
                {
                    let pc = game_boy.register_pair(RegisterPair::PC);

                    if let Some(break_point) = egui.break_point {
                        if pc == break_point {
                            step_mode = true;
                            break;
                        }
                    }
                }
            }

            cycles_in_frame += elapsed_cycles;

            if cycles_in_frame >= CYCLES_IN_FRAME {
                // Redraw
                cycles_in_frame = Default::default();

                game_boy.get_ppu().copy_to_gui(pixels.get_frame());
                window.request_redraw();
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
        .build(event_loop)?)
}
