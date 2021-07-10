use anyhow::{anyhow, Result};
use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use gb::{AudioSenderReceiver, Cycle, Egui, GB_HEIGHT, GB_WIDTH};
use gilrs::Gilrs;
use pixels::{Pixels, SurfaceTexture};
use rodio::{OutputStream, Sink};
use std::time::Instant;
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};
use winit_input_helper::WinitInputHelper;

const SCALE: f64 = 2.0;

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

    // `rom` is a required value in every situation so this will
    // always exist.
    let rom_path = m
        .value_of("rom")
        .expect("Required value 'rom' was provided");

    let mut game_boy =
        gb::emu::init(m.value_of("boot"), rom_path).expect("Initialized DMG-01 Emulator");
    let cartridge_title = gb::emu::rom_title(&game_boy);

    // Initialize Gamepad Support
    let mut gamepad = Gilrs::new().expect("Initialized Gilrs for Controller Input");

    // Initialize GUI
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = create_window(&event_loop, cartridge_title)?;

    let (mut pixels, mut egui) = {
        let size = window.inner_size();
        let scale_factor = window.scale_factor();
        let surface_texture = SurfaceTexture::new(size.width, size.height, &window);
        let pixels = Pixels::new(GB_WIDTH as u32, GB_HEIGHT as u32, surface_texture)?;
        let egui = Egui::new(size.width, size.height, scale_factor, pixels.context());

        (pixels, egui)
    };

    let (send, recv) = AudioSenderReceiver::new();
    game_boy.set_audio_src(send);

    // Initialize Audio
    let (_stream, stream_handle) = OutputStream::try_default().expect("Initialized Audio");
    let sink = Sink::try_new(&stream_handle).expect("Initialize Audio Sink");

    std::thread::spawn(move || {
        sink.append(recv);

        sink.sleep_until_end();
    });

    let mut now = Instant::now();
    let mut cycle_count: Cycle = Default::default();

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

            if let Some(scale_factor) = input.scale_factor() {
                egui.scale_factor(scale_factor);
            }

            if let Some(size) = input.window_resized() {
                pixels.resize_surface(size.width, size.height);
                egui.resize(size.width, size.height);
            }

            let delta = now.elapsed().subsec_nanos();
            now = Instant::now();

            let pending = Cycle::new(delta / gb::emu::SM83_CYCLE_TIME.subsec_nanos());
            cycle_count += gb::emu::run(&mut game_boy, &mut gamepad, &input, pending);

            if cycle_count >= gb::emu::CYCLES_IN_FRAME {
                // Draw Frame
                cycle_count = Cycle::new(0);

                gb::emu::draw(game_boy.ppu(), pixels.get_frame());
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
        .with_resizable(true)
        .with_decorations(true)
        .with_transparent(false)
        // .with_drag_and_drop(false) // OleInitialize failed error if this is set to true
        .build(event_loop)?)
}
