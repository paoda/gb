use anyhow::{anyhow, Result};
use gb::cpu::Cpu as LR35902;
use pixels::{Pixels, SurfaceTexture};
use std::io::Write;
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

    let out = Box::leak(Box::new(std::io::stdout()));
    let mut out_handle = out.lock();
    let mut game_boy = LR35902::new();
    game_boy.load_cartridge("bin/cpu_instrs.gb");

    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            let ppu = game_boy.get_ppu();
            let frame = pixels.get_frame();

            ppu.draw(frame);

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
            let addr = game_boy.register_pair(gb::cpu::RegisterPair::PC);

            let opcode = game_boy.fetch();
            let instruction = game_boy.decode(opcode);
            let _cycles = game_boy.execute(instruction);
            window.request_redraw();

            write!(
                out_handle,
                "Addr: {:#06X} | Opcode: {:#04X} | Instr: {:X?}\n",
                addr, opcode, instruction
            )
            .unwrap();
        }
    });

    // loop {
    //     let pc = game_boy.register_pair(gb::cpu::RegisterPair::PC);
    //     let opcode = game_boy.fetch();
    //     let instruction = game_boy.decode(opcode);

    //     let _cycles = game_boy.execute(instruction);

    //     let ppu = game_boy.get_ppu();
    //     ppu.step();
    // }
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
