use std::time::Instant;

use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};
use egui_wgpu_backend::RenderPass;
use gb::emu::Emulator;
use gb::gui::GuiState;
use gilrs::Gilrs;
use rodio::{OutputStream, Sink};
use tracing_subscriber::EnvFilter;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};

const AUDIO_ENABLED: bool = true;

fn main() {
    let app = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!());

    let m = app
        .arg(
            Arg::with_name("rom")
                .value_name("ROM_FILE")
                .takes_value(true)
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

    // --Here lies a lot of Winit + WGPU Boilerplate--
    let event_loop: EventLoop<Event<()>> = EventLoop::with_user_event();
    let window = gb::gui::build_window(&event_loop).expect("build window");

    let (instance, surface) = gb::gui::create_surface(&window);
    let adapter = gb::gui::request_adapter(&instance, &surface).expect("request adaptor");
    let (device, queue) = gb::gui::request_device(&adapter).expect("request device");
    let format = surface
        .get_preferred_format(&adapter)
        .expect("get surface format");

    let mut config = gb::gui::surface_config(&window, format);
    surface.configure(&device, &config);
    let mut platform = gb::gui::platform_desc(&window);
    let mut render_pass = RenderPass::new(&device, format, 1);

    // We interrupt your boiler plate to initialize the emulator so that
    // we can copy it's empty pixel buffer to the GPU
    let mut emu = match m.value_of("boot") {
        Some(path) => {
            tracing::info!("User-provided boot ROM");
            Emulator::from_boot_rom(path).expect("initialize emulator with custom boot rom")
        }
        None => {
            tracing::info!("Built-in boot ROM");
            Emulator::new()
        }
    };

    // Set up the WGPU (and then EGUI) texture we'll be working with.
    let texture_size = gb::gui::texture_size();
    let texture = gb::gui::create_texture(&device, texture_size);
    gb::gui::write_to_texture(&queue, &texture, gb::emu::pixel_buf(&emu), texture_size);
    let texture_id = gb::gui::expose_texture_to_egui(&mut render_pass, &device, &texture);

    // Load ROM if filepath was provided
    if let Some(path) = m.value_of("rom") {
        tracing::info!("User-provided cartridge ROM");
        emu.read_game_rom(path).expect("read game rom from path");
    }

    // Load Save File if it exists
    // FIXME: Shouldn't the API be better than this?
    emu.try_load_sav().expect("Load save if exists");
    let rom_title = emu.title().to_string();

    tracing::info!("Initialize Gamepad");
    let mut gamepad = Gilrs::new().expect("Initialize Controller Support");

    // Initialize Audio
    let (_stream, stream_handle) = OutputStream::try_default().expect("Initialized Audio");

    if AUDIO_ENABLED {
        let (prod, cons) = gb::spsc_init();
        let sink = {
            let s = Sink::try_new(&stream_handle).expect("create sink from audio stream handle");
            s.append(cons);
            s.set_volume(0.1);
            s
        };

        emu.set_prod(prod);

        tracing::info!("Spawn Audio Thread");
        std::thread::spawn(move || {
            sink.sleep_until_end();
        });
    }

    // Set up state for the Immediate-mode GUI
    let mut app = GuiState::new(rom_title);
    let mut last_key = gb::gui::unused_key();

    // used for egui animations
    let start_time = Instant::now();

    event_loop.run(move |event, _, control_flow| {
        platform.handle_event(&event);

        match event {
            Event::MainEventsCleared => {
                if app.quit {
                    *control_flow = ControlFlow::Exit;
                }

                gb::emu::run_frame(&mut emu, &mut gamepad, last_key);

                window.request_redraw();
            }
            Event::RedrawRequested(..) => {
                platform.update_time(start_time.elapsed().as_secs_f64());

                let data = gb::emu::pixel_buf(&emu);
                gb::gui::write_to_texture(&queue, &texture, data, texture_size);

                let output_frame = match surface.get_current_texture() {
                    Ok(frame) => frame,
                    Err(e) => {
                        eprintln!("Dropped frame with error: {}", e);
                        return;
                    }
                };
                let output_view = gb::gui::create_view(&output_frame);

                // Begin to draw Egui components
                platform.begin_frame();
                gb::gui::draw_egui(&mut app, &platform.context(), texture_id);
                // End the UI frame. We could now handle the output and draw the UI with the backend.
                let (_, paint_commands) = platform.end_frame(Some(&window));
                let paint_jobs = platform.context().tessellate(paint_commands);

                let mut encoder = gb::gui::create_command_encoder(&device);
                let screen_descriptor = gb::gui::create_screen_descriptor(&window, &config);

                // Upload all resources for the GPU.
                render_pass.update_texture(&device, &queue, &platform.context().texture());
                render_pass.update_user_textures(&device, &queue);
                render_pass.update_buffers(&device, &queue, &paint_jobs, &screen_descriptor);

                // Record all render passes.
                gb::gui::execute_render_pass(
                    &mut render_pass,
                    &mut encoder,
                    &output_view,
                    paint_jobs,
                    &screen_descriptor,
                )
                .expect("record render passes");

                // Submit the commands.
                queue.submit(std::iter::once(encoder.finish()));

                // Redraw egui
                output_frame.present();
            }
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(size) => {
                    config.width = size.width;
                    config.height = size.height;
                    surface.configure(&device, &config);
                }
                WindowEvent::CloseRequested => {
                    *control_flow = ControlFlow::Exit;
                }
                WindowEvent::KeyboardInput { input, .. } => last_key = input,
                _ => {}
            },
            _ => {}
        }
    });
}
