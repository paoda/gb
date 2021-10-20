use std::convert::TryInto;

use anyhow::Result;
use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use gb::emu::build::EmulatorBuilder;
use gb::emu::CYCLES_IN_FRAME;
use gb::{Cycle, GB_HEIGHT, GB_WIDTH};
use gilrs::Gilrs;
use pixels::{PixelsBuilder, SurfaceTexture};
use rodio::{OutputStream, Sink};
use tracing::info;
use tracing_subscriber::EnvFilter;
use winit::dpi::{LogicalSize, PhysicalSize};
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};
use winit_input_helper::WinitInputHelper;

const WINDOW_SCALE: usize = 3;
const AUDIO_ENABLED: bool = true;

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

    // Set up subscriber
    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "gb=info");
    }

    tracing_subscriber::fmt::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let mut emu_build =
        EmulatorBuilder::new().with_cart(m.value_of("rom").expect("ROM path provided"))?;

    if let Some(path) = m.value_of("boot") {
        emu_build = emu_build.with_boot(path)?;
    }

    let mut emu = emu_build.finish();

    // Load Save file if it exists
    info!("Attempt to load .sav");
    emu.try_load_sav().expect("Load save if exists");
    let rom_title = emu.title();

    info!("Initialize Gamepad");
    let mut gamepad = Gilrs::new().expect("Initialize Controller Support");

    // Initialize GUI
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = create_window(&event_loop, rom_title)?;

    let mut pixels = {
        let size = window.inner_size();
        let surface_texture = SurfaceTexture::new(size.width, size.height, &window);

        PixelsBuilder::new(GB_WIDTH as u32, GB_HEIGHT as u32, surface_texture)
            .enable_vsync(false)
            .build()?
    };

    // Initialize Audio
    let (_stream, stream_handle) = OutputStream::try_default().expect("Initialized Audio");

    if AUDIO_ENABLED {
        let (prod, cons) = gb::spsc_init();
        let sink = {
            let s = Sink::try_new(&stream_handle)?;
            s.append(cons);
            s.set_volume(0.1);
            s
        };

        emu.set_prod(prod);

        info!("Spawn Audio Thread");
        std::thread::spawn(move || {
            sink.sleep_until_end();
        });
    }

    let mut cycle_count: Cycle = Default::default();

    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            if pixels.render().is_err() {
                emu.try_write_sav().expect("Write game save if need be");

                *control_flow = ControlFlow::Exit;
                return;
            }
        }

        if input.update(&event) {
            if input.key_pressed(VirtualKeyCode::Escape) || input.quit() {
                emu.try_write_sav().expect("Write game save if need be");

                *control_flow = ControlFlow::Exit;
                return;
            }

            if let Some(size) = input.window_resized() {
                pixels.resize_surface(size.width, size.height);
            }

            cycle_count += gb::emu::run_frame(&mut emu, &mut gamepad, &input);

            if cycle_count >= CYCLES_IN_FRAME {
                cycle_count %= CYCLES_IN_FRAME;

                let buf: &mut [u8; GB_WIDTH * GB_HEIGHT * 4] = pixels
                    .get_frame()
                    .try_into()
                    .expect("Size of Pixel Buffer is GB_WIDTH * GB_HEIGHT * 4");

                gb::emu::draw_frame(&emu, buf);
                window.request_redraw();
            }
        }
    });
}

fn create_window(event_loop: &EventLoop<()>, title: &str) -> Result<Window> {
    let logical = LogicalSize::new(GB_WIDTH as f64, GB_HEIGHT as f64);
    let physical = PhysicalSize::new(
        (GB_WIDTH * WINDOW_SCALE) as f32,
        (GB_HEIGHT * WINDOW_SCALE) as f32,
    );

    Ok(WindowBuilder::new()
        .with_title(title)
        .with_min_inner_size(logical)
        .with_inner_size(physical)
        .with_resizable(true)
        .build(event_loop)?)
}
