use egui::{ClippedMesh, CtxRef, TextureId};
use egui_wgpu_backend::{BackendError, RenderPass, ScreenDescriptor};
use egui_winit_platform::Platform;
use wgpu::{
    Adapter, CommandEncoder, Device, Extent3d, FilterMode, Instance, Queue, RequestDeviceError,
    Surface, SurfaceConfiguration, SurfaceTexture, Texture, TextureFormat, TextureUsages,
    TextureView,
};
use winit::error::OsError;
use winit::event::{ElementState, KeyboardInput};
use winit::event_loop::EventLoop;
use winit::window::Window;

use crate::cpu::Cpu;
use crate::{GB_HEIGHT, GB_WIDTH};

const EGUI_DIMENSIONS: (usize, usize) = (1280, 720);
const FILTER_MODE: FilterMode = FilterMode::Nearest;
const WINDOW_TITLE: &str = "DMG-01 Emulator";

const SCALE: f32 = 3.0;

/// Holds GUI State
#[derive(Debug, Clone)]
pub struct GuiState {
    /// When true, egui winit should exit the application
    pub quit: bool,
    pub title: String,
    pub mode: EmuMode,
}

impl GuiState {
    pub fn new(title: String) -> Self {
        Self {
            title,
            quit: Default::default(),
            mode: EmuMode::Running,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmuMode {
    Running,
    StepFrame,
    Stopped,
    Step,
}

/// To avoid using an [Option<KeyboardInput>] to keep track of user input from winit,
/// we can use a "default" value. However, in order for this to work the chosen "default"
/// value must be an **unused** key, so that it is ignored by the emulator.
pub fn unused_key() -> KeyboardInput {
    #![allow(deprecated)]

    KeyboardInput {
        scancode: Default::default(),
        state: ElementState::Released,
        virtual_keycode: Default::default(),
        modifiers: Default::default(), // this argument is deprecated
    }
}

pub fn build_window<T>(event_loop: &EventLoop<T>) -> Result<Window, OsError> {
    use winit::dpi::PhysicalSize;
    use winit::window::WindowBuilder;

    WindowBuilder::new()
        .with_decorations(true)
        .with_resizable(true)
        .with_transparent(false)
        .with_title(WINDOW_TITLE)
        .with_inner_size(PhysicalSize {
            width: EGUI_DIMENSIONS.0 as f32,
            height: EGUI_DIMENSIONS.1 as f32,
        })
        .build(event_loop)
}

pub fn create_surface(window: &Window) -> (Instance, Surface) {
    use wgpu::Backends;

    let instance = Instance::new(Backends::PRIMARY);
    let surface = unsafe { instance.create_surface(window) };
    (instance, surface)
}

pub fn request_adapter(instance: &Instance, surface: &Surface) -> Option<Adapter> {
    use wgpu::{PowerPreference, RequestAdapterOptions};

    pollster::block_on(instance.request_adapter(&RequestAdapterOptions {
        power_preference: PowerPreference::HighPerformance,
        force_fallback_adapter: false, // TODO: What do I want to do with this?
        compatible_surface: Some(surface),
    }))
}

pub fn request_device(adapter: &Adapter) -> Result<(Device, Queue), RequestDeviceError> {
    use wgpu::{DeviceDescriptor, Features, Limits};

    pollster::block_on(adapter.request_device(
        &DeviceDescriptor {
            label: None,
            features: Features::default(),
            limits: Limits::default(),
        },
        None,
    ))
}

pub fn surface_config(window: &Window, format: TextureFormat) -> SurfaceConfiguration {
    use wgpu::PresentMode;

    let size = window.inner_size();
    SurfaceConfiguration {
        usage: TextureUsages::RENDER_ATTACHMENT,
        format,
        width: size.width as u32,
        height: size.height as u32,
        present_mode: PresentMode::Immediate,
    }
}

pub fn platform_desc(window: &Window) -> Platform {
    use egui::FontDefinitions;
    use egui_winit_platform::PlatformDescriptor;

    let size = window.inner_size();
    Platform::new(PlatformDescriptor {
        physical_width: size.width as u32,
        physical_height: size.height as u32,
        scale_factor: window.scale_factor(),
        font_definitions: FontDefinitions::default(),
        ..Default::default()
    })
}

pub fn texture_size() -> Extent3d {
    Extent3d {
        width: GB_WIDTH as u32,
        height: GB_HEIGHT as u32,
        ..Default::default()
    }
}

pub fn create_texture(device: &Device, size: Extent3d) -> Texture {
    use wgpu::{TextureDescriptor, TextureDimension};

    device.create_texture(&TextureDescriptor {
        size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: TextureDimension::D2,
        format: TextureFormat::Rgba8UnormSrgb,
        usage: TextureUsages::COPY_DST | TextureUsages::TEXTURE_BINDING,
        label: Some("gb_pixel_buffer"),
    })
}

#[inline]
pub fn write_to_texture(
    queue: &Queue,
    texture: &Texture,
    data: &[u8; GB_WIDTH * 4 * GB_HEIGHT],
    size: Extent3d,
) {
    use std::num::NonZeroU32;
    use wgpu::{ImageCopyTexture, ImageDataLayout, Origin3d, TextureAspect};

    queue.write_texture(
        ImageCopyTexture {
            texture,
            mip_level: 0,
            origin: Origin3d::ZERO,
            aspect: TextureAspect::All,
        },
        data,
        ImageDataLayout {
            offset: 0,
            bytes_per_row: NonZeroU32::new(4 * GB_WIDTH as u32),
            rows_per_image: NonZeroU32::new(GB_HEIGHT as u32),
        },
        size,
    );
}

pub fn expose_texture_to_egui(
    render_pass: &mut RenderPass,
    device: &Device,
    texture: &Texture,
) -> TextureId {
    render_pass.egui_texture_from_wgpu_texture(device, texture, FILTER_MODE)
}

#[inline]
pub fn create_view(frame: &SurfaceTexture) -> TextureView {
    use wgpu::TextureViewDescriptor;

    frame.texture.create_view(&TextureViewDescriptor::default())
}

#[inline]
pub fn create_command_encoder(device: &Device) -> CommandEncoder {
    use wgpu::CommandEncoderDescriptor;

    device.create_command_encoder(&CommandEncoderDescriptor {
        label: Some("encoder"),
    })
}

#[inline]
pub fn create_screen_descriptor(
    window: &Window,
    config: &SurfaceConfiguration,
) -> ScreenDescriptor {
    ScreenDescriptor {
        physical_width: config.width,
        physical_height: config.height,
        scale_factor: window.scale_factor() as f32,
    }
}

#[inline]
pub fn execute_render_pass(
    render_pass: &mut RenderPass,
    encoder: &mut CommandEncoder,
    view: &TextureView,
    jobs: Vec<ClippedMesh>,
    descriptor: &ScreenDescriptor,
) -> Result<(), BackendError> {
    render_pass.execute(encoder, view, &jobs, descriptor, Some(wgpu::Color::BLACK))
}

#[inline]
pub fn draw_egui(cpu: &Cpu, app: &mut GuiState, ctx: &CtxRef, texture_id: TextureId) {
    use crate::{cpu, instruction, ppu};

    fn selectable_text(ui: &mut egui::Ui, mut text: &str) -> egui::Response {
        ui.add(egui::TextEdit::multiline(&mut text).code_editor())
    }

    egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
        egui::menu::menu(ui, "File", |ui| {
            if ui.button("Quit").clicked() {
                app.quit = true;
            }
        });

        egui::Window::new(&app.title).show(ctx, |ui| {
            ui.image(
                texture_id,
                [GB_WIDTH as f32 * SCALE, GB_HEIGHT as f32 * SCALE],
            );
        });

        egui::Window::new("Disassembly").show(ctx, |ui| {
            selectable_text(ui, &instruction::dbg::tmp_disasm(cpu, 20));
        });

        egui::Window::new("Settings").show(ctx, |ui| {
            egui::ComboBox::from_label("Emulation Mode")
                .selected_text(format!("{:?}", app.mode))
                .show_ui(ui, |ui| {
                    ui.selectable_value(&mut app.mode, EmuMode::Running, "Running");
                    ui.selectable_value(&mut app.mode, EmuMode::Stopped, "Stopped");
                    ui.selectable_value(&mut app.mode, EmuMode::StepFrame, "Step Frame");
                    ui.selectable_value(&mut app.mode, EmuMode::Step, "Step");
                })
        });

        egui::Window::new("GB Info").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    ui.heading("CPU");

                    ui.monospace(format!("AF: {:#06X}", cpu::dbg::af(cpu)));
                    ui.monospace(format!("BC: {:#06X}", cpu::dbg::bc(cpu)));
                    ui.monospace(format!("DE: {:#06X}", cpu::dbg::de(cpu)));
                    ui.monospace(format!("HL: {:#06X}", cpu::dbg::hl(cpu)));
                    ui.add_space(10.0);
                    ui.monospace(format!("SP: {:#06X}", cpu::dbg::sp(cpu)));
                    ui.monospace(format!("PC: {:#06X}", cpu::dbg::pc(cpu)));
                });

                ui.vertical(|ui| {
                    let ppu = &cpu.bus.ppu;

                    ui.heading("PPU");

                    ui.monospace(format!("LY: {}", ppu::dbg::ly(ppu)));
                    ui.horizontal(|ui| {
                        ui.monospace(format!("SCX: {}", ppu::dbg::scx(ppu)));
                        ui.monospace(format!("SCY: {}", ppu::dbg::scy(ppu)));
                    });
                    ui.horizontal(|ui| {
                        ui.monospace(format!("WX: {}", ppu::dbg::wx(ppu)));
                        ui.monospace(format!("WY: {}", ppu::dbg::wy(ppu)));
                    });

                    ui.monospace(format!(
                        "Mode: {:?} {}",
                        ppu::dbg::mode(ppu),
                        ppu::dbg::dot(ppu)
                    ))
                });
            });

            ui.add_space(10.0);

            let _ = ui.selectable_label(cpu::dbg::ime(cpu), "IME");
            ui.horizontal(|ui| {
                let irq = cpu.int_request();

                ui.label("IRQ:");
                let _ = ui.selectable_label(irq & 0b01 == 0x01, "VBlank");
                let _ = ui.selectable_label(irq >> 1 & 0x01 == 0x01, "LCD STAT");
                let _ = ui.selectable_label(irq >> 2 & 0x01 == 0x01, "Timer");
                let _ = ui.selectable_label(irq >> 3 & 0x01 == 0x01, "Serial");
                let _ = ui.selectable_label(irq >> 4 & 0x01 == 0x01, "Joypad");
            });

            ui.horizontal(|ui| {
                let ie = cpu.int_enable();

                let r_len = ctx.fonts().glyph_width(egui::TextStyle::Body, 'R');
                let e_len = ctx.fonts().glyph_width(egui::TextStyle::Body, 'E');
                let q_len = ctx.fonts().glyph_width(egui::TextStyle::Body, 'Q');

                ui.label("IE:");
                ui.add_space(q_len - (e_len - r_len));
                let _ = ui.selectable_label(ie & 0b01 == 0x01, "VBlank");
                let _ = ui.selectable_label(ie >> 1 & 0x01 == 0x01, "LCD STAT");
                let _ = ui.selectable_label(ie >> 2 & 0x01 == 0x01, "Timer");
                let _ = ui.selectable_label(ie >> 3 & 0x01 == 0x01, "Serial");
                let _ = ui.selectable_label(ie >> 4 & 0x01 == 0x01, "Joypad");
            });

            ui.add_space(10.0);

            ui.horizontal(|ui| {
                let flags = cpu::dbg::flags(cpu);

                let _ = ui.selectable_label((flags >> 7 & 0x01) == 0x01, "Zero");
                let _ = ui.selectable_label((flags >> 6 & 0x01) == 0x01, "Negative");
                let _ = ui.selectable_label((flags >> 5 & 0x01) == 0x01, "Half-Carry");
                let _ = ui.selectable_label((flags >> 4 & 0x01) == 0x01, "Carry");
            })
        })
    });
}

pub mod kbd {
    use winit::event::{ElementState, KeyboardInput, VirtualKeyCode};

    pub fn space_released(input: &KeyboardInput) -> bool {
        let keycode = input.virtual_keycode;
        matches!(input.state, ElementState::Released if keycode == Some(VirtualKeyCode::Space))
    }
}
