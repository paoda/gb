use std::time::Instant;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use egui_wgpu_backend::RenderPass;
use gb::gui::EmuMode;
use gb::{emu, gui};
use gilrs::Gilrs;
use gui::GuiState;
use rodio::{OutputStream, Sink};
use tracing_subscriber::EnvFilter;
use winit::event::{Event, WindowEvent};
use winit::event_loop::EventLoop;

const AUDIO_ENABLED: bool = true;

fn main() {
    let app = Command::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!());

    let m = app
        .arg(
            Arg::new("rom")
                .value_name("ROM_FILE")
                .takes_value(true)
                .index(1)
                .help("Path to the Game ROM"),
        )
        .arg(
            Arg::new("boot")
                .short('b')
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

    // --Here lies a lot of winit + wgpu Boilerplate--
    let event_loop: EventLoop<Event<()>> = EventLoop::with_user_event();
    let window = gui::build_window(&event_loop).expect("build window");

    let (instance, surface) = gui::create_surface(&window);
    let adapter = gui::request_adapter(&instance, &surface).expect("request adaptor");
    let (device, queue) = gui::request_device(&adapter).expect("request device");
    let format = surface
        .get_preferred_format(&adapter)
        .expect("get surface format");

    let mut config = gui::surface_config(&window, format);
    surface.configure(&device, &config);
    let mut platform = gui::platform_desc(&window);
    let mut render_pass = RenderPass::new(&device, format, 1);

    // We interrupt your boiler plate to initialize the emulator so that
    // we can copy it's empty pixel buffer to the GPU
    let mut cpu = match m.value_of("boot") {
        Some(path) => {
            tracing::info!("User-provided boot ROM");
            emu::from_boot_rom(path).expect("initialize emulator with custom boot rom")
        }
        None => {
            tracing::info!("Built-in boot ROM");
            Default::default()
        }
    };

    // Set up the wgpu (and then EGUI) texture we'll be working with.
    let texture_size = gui::texture_size();
    let texture = gui::create_texture(&device, texture_size);
    gui::write_to_texture(&queue, &texture, emu::pixel_buf(&cpu), texture_size);
    let texture_id = gui::expose_texture_to_egui(&mut render_pass, &device, &texture);

    // Load ROM if filepath was provided
    if let Some(path) = m.value_of("rom") {
        tracing::info!("User-provided cartridge ROM");
        emu::read_game_rom(&mut cpu, path).expect("read game rom from path");
    }

    emu::load_save(&mut cpu);

    let rom_title = emu::rom_title(&cpu).to_string();

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

        emu::set_audio_prod(&mut cpu, prod);

        tracing::info!("Spawn Audio Thread");
        std::thread::spawn(move || {
            sink.sleep_until_end();
        });
    }

    // Set up state for the Immediate-mode GUI
    let mut app = GuiState::new(rom_title);
    let mut last_key = gui::unused_key();

    // used for egui animations
    let start_time = Instant::now();

    event_loop.run(move |event, _, control_flow| {
        platform.handle_event(&event);

        match event {
            Event::MainEventsCleared => {
                if app.quit {
                    emu::save_and_exit(&cpu, control_flow);
                }

                match app.mode {
                    EmuMode::Running => emu::run_frame(&mut cpu, &mut gamepad, last_key),
                    EmuMode::StepFrame if gui::kbd::space_released(&last_key) => {
                        emu::run_frame(&mut cpu, &mut gamepad, last_key)
                    }
                    EmuMode::Step if gui::kbd::space_released(&last_key) => {
                        emu::run(&mut cpu, &mut gamepad, last_key, 4);
                    }
                    _ => {}
                };

                // Input has been consumed, reset it
                last_key = gui::unused_key();

                window.request_redraw();
            }
            Event::RedrawRequested(..) => {
                platform.update_time(start_time.elapsed().as_secs_f64());

                let data = emu::pixel_buf(&cpu);
                gui::write_to_texture(&queue, &texture, data, texture_size);

                let output_frame = match surface.get_current_texture() {
                    Ok(frame) => frame,
                    Err(e) => {
                        eprintln!("Dropped frame with error: {}", e);
                        return;
                    }
                };
                let output_view = gui::create_view(&output_frame);

                // Begin to draw Egui components
                platform.begin_frame();
                gui::draw_egui(&cpu, &mut app, &platform.context(), texture_id);
                // End the UI frame. We could now handle the output and draw the UI with the backend.
                let (_, paint_commands) = platform.end_frame(Some(&window));
                let paint_jobs = platform.context().tessellate(paint_commands);

                let mut encoder = gui::create_command_encoder(&device);
                let screen_descriptor = gui::create_screen_descriptor(&window, &config);

                // Upload all resources for the GPU.
                render_pass.update_texture(&device, &queue, &platform.context().texture());
                render_pass.update_user_textures(&device, &queue);
                render_pass.update_buffers(&device, &queue, &paint_jobs, &screen_descriptor);

                // Record all render passes.
                gui::execute_render_pass(
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
                    emu::save_and_exit(&cpu, control_flow);
                }
                WindowEvent::KeyboardInput { input, .. } => last_key = input,
                _ => {}
            },
            _ => {}
        }
    });
}
