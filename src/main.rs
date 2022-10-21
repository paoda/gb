use std::time::Instant;

use clap::{crate_authors, crate_description, crate_name, crate_version, Arg, Command};
use gb::gui::{EmuMode, Gui};
use gb::{emu, gui};
use gilrs::Gilrs;
use rodio::{OutputStream, Sink};
use tracing_subscriber::EnvFilter;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{EventLoop, EventLoopBuilder};

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

    // Init CPU
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
    let event_loop: EventLoop<Event<()>> = EventLoopBuilder::with_user_event().build();

    let mut app = Gui::new(rom_title, &event_loop, &cpu);
    let mut last_key = gui::unused_key(); // TODO: Fix this awful impl

    // used for egui animations
    let start_time = Instant::now();

    event_loop.run(move |event, _, control_flow| {
        app.handle_event(&event);

        match event {
            Event::MainEventsCleared => {
                app.maybe_quit(&cpu, control_flow);

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

                app.request_redraw();
            }
            Event::RedrawRequested(..) => {
                app.update_time(start_time.elapsed().as_secs_f64());
                app.paint(&cpu);
            }
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(size) => app.resize(size),
                WindowEvent::CloseRequested => emu::save_and_exit(&cpu, control_flow),
                WindowEvent::KeyboardInput { input, .. } => last_key = input,
                _ => {}
            },
            _ => {}
        }
    });
}
