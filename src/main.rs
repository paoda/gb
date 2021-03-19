use anyhow::{anyhow, Result};
use gb::cpu::Cpu as LR35902;
use pixels::{Pixels, SurfaceTexture};
use std::env::args;
use winit::{
    dpi::LogicalSize,
    event::{Event, VirtualKeyCode},
    event_loop::{ControlFlow, EventLoop},
    window::Window,
    window::WindowBuilder,
};
use winit_input_helper::WinitInputHelper;

// 160 x 144
const GB_WIDTH: u32 = 160;
const GB_HEIGHT: u32 = 144;

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

            // Emulation
            let _cycles = game_boy.step();

            let ppu = game_boy.get_ppu();
            let frame = pixels.get_frame();
            ppu.copy_to_gui(frame);
            window.request_redraw();
        }
    });
}

pub fn create_window(event_loop: &EventLoop<()>) -> Result<Window> {
    let size = LogicalSize::new(GB_WIDTH as f64, GB_HEIGHT as f64);
    Ok(WindowBuilder::new()
        .with_title("DMG-1 Game Boy")
        .with_inner_size(size)
        .with_min_inner_size(size)
        .build(&event_loop)?)
}

pub fn create_pixels(window: &Window) -> Result<Pixels<Window>> {
    let window_size = window.inner_size();
    let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, window);
    Ok(Pixels::new(GB_WIDTH, GB_HEIGHT, surface_texture)?)
}
