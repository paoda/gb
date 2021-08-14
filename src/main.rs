use anyhow::{anyhow, Result};
use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use gb::{AudioSPSC, Cycle, GB_HEIGHT, GB_WIDTH};
use gilrs::Gilrs;
use pixels::{PixelsBuilder, SurfaceTexture};
use rodio::{OutputStream, Sink};
use std::time::{Duration, Instant};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};
use winit_input_helper::WinitInputHelper;

const WINDOW_SCALE: f64 = 2.0;

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

    let rom_path = m
        .value_of("rom")
        .expect("Required value 'rom' was provided");

    let mut game_boy =
        gb::emu::init(m.value_of("boot"), rom_path).expect("Initialize DMG-01 Emulator");
    let rom_title = gb::emu::rom_title(&game_boy);

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
    let spsc: AudioSPSC<f32> = Default::default();
    let (prod, cons) = spsc.init();
    let (_stream, stream_handle) = OutputStream::try_default().expect("Initialized Audio");
    let sink = Sink::try_new(&stream_handle)?;
    sink.append(cons);
    game_boy.apu_mut().set_producer(prod);

    std::thread::spawn(move || {
        sink.sleep_until_end();
    });

    let mut start = Instant::now();
    let frame_time = Duration::from_secs_f64(1.0 / 60.0); // 60 Hz on Host
    let mut cycle_count: Cycle = Default::default();

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
                pixels.resize_surface(size.width, size.height);
            }

            let mut diff = Instant::now() - start;
            while diff.subsec_nanos() < frame_time.subsec_nanos() {
                if cycle_count < gb::emu::CYCLES_IN_FRAME {
                    cycle_count += gb::emu::run_frame(&mut game_boy, &mut gamepad, &input);
                }

                diff = Instant::now() - start;
            }
            start = Instant::now();

            if cycle_count >= gb::emu::CYCLES_IN_FRAME {
                cycle_count %= gb::emu::CYCLES_IN_FRAME;

                gb::emu::draw(game_boy.ppu(), pixels.get_frame());
                window.request_redraw();
            }
        }
    });
}

#[cfg(not(windows))]
fn create_window(event_loop: &EventLoop<()>, title: &str) -> Result<Window> {
    let size = LogicalSize::new(
        (GB_WIDTH as f64) * WINDOW_SCALE,
        (GB_HEIGHT as f64) * WINDOW_SCALE,
    );
    Ok(WindowBuilder::new()
        .with_title(title)
        .with_inner_size(size)
        .with_min_inner_size(size)
        .with_resizable(true)
        .with_decorations(true)
        .with_transparent(false)
        .build(event_loop)?)
}

#[cfg(windows)]
fn create_window(event_loop: &EventLoop<()>, title: &str) -> Result<Window> {
    use winit::platform::windows::WindowBuilderExtWindows;

    let size = LogicalSize::new(
        (GB_WIDTH as f64) * WINDOW_SCALE,
        (GB_HEIGHT as f64) * WINDOW_SCALE,
    );
    Ok(WindowBuilder::new()
        .with_title(title)
        .with_inner_size(size)
        .with_min_inner_size(size)
        .with_resizable(true)
        .with_decorations(true)
        .with_transparent(false)
        .with_drag_and_drop(false)
        .build(event_loop)?)
}
