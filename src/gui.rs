use egui::{Context, TextureId};
use egui_wgpu_backend::{RenderPass, ScreenDescriptor};
use egui_winit_platform::Platform;
use wgpu::{
    Adapter, Backends, Color, CommandEncoder, Device, Extent3d, FilterMode, Instance, Queue,
    RequestDeviceError, Surface, SurfaceConfiguration, Texture, TextureFormat, TextureUsages,
    TextureView, TextureViewDescriptor, CompositeAlphaMode,
};
use winit::dpi::PhysicalSize;
use winit::error::OsError;
use winit::event::{ElementState, Event, KeyboardInput};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::Window;

use crate::cpu::Cpu;
use crate::{emu, GB_HEIGHT, GB_WIDTH};

const EGUI_DIMENSIONS: (usize, usize) = (1280, 720);
const FILTER_MODE: FilterMode = FilterMode::Nearest;
const WINDOW_TITLE: &str = "DMG-01 Emulator";

const SCALE: f32 = 3.0;

pub struct Gui {
    pub should_quit: bool,
    pub title: String,
    pub mode: EmuMode,

    window: Window,
    platform: Platform,

    surface: Surface,
    device: Device,
    queue: Queue,
    surface_config: SurfaceConfiguration,
    render_pass: RenderPass,

    texture: Texture,
    texture_size: Extent3d,
    texture_id: TextureId,
}

impl Gui {
    pub fn new<T>(title: String, event_loop: &EventLoop<T>, cpu: &Cpu) -> Self {
        let window = build_window(event_loop).expect("build window");

        let instance = Instance::new(Backends::PRIMARY);
        let surface = unsafe { instance.create_surface(&window) };

        let adapter = request_adapter(&instance, &surface).expect("request adaptor");
        let (device, queue) = request_device(&adapter).expect("request device");
        let texture_format = surface.get_supported_formats(&adapter)[0]; // First is preferred

        let alpha_mode = surface.get_supported_alpha_modes(&adapter)[0];
        let surface_config = surface_config(&window, alpha_mode, texture_format);
        surface.configure(&device, &surface_config);
        let platform = platform(&window);
        let mut render_pass = RenderPass::new(&device, texture_format, 1);

        let texture_size = texture_size();
        let texture = create_texture(&device, texture_size);
        write_to_texture(&queue, &texture, emu::pixel_buf(cpu), texture_size);

        let view = texture.create_view(&TextureViewDescriptor::default());
        let texture_id = expose_texture_to_egui(&mut render_pass, &device, &view);

        Self {
            should_quit: Default::default(),
            title,
            mode: EmuMode::Running,

            window,
            platform,

            surface,
            device,
            queue,
            surface_config,
            render_pass,

            texture,
            texture_size,
            texture_id,
        }
    }

    pub fn maybe_quit(&self, cpu: &Cpu, control_flow: &mut ControlFlow) {
        if self.should_quit {
            emu::save_and_exit(cpu, control_flow);
        }
    }

    pub fn request_redraw(&self) {
        self.window.request_redraw();
    }

    pub fn handle_event<T>(&mut self, event: &Event<T>) {
        self.platform.handle_event(event);
    }

    pub fn update_time(&mut self, elapsed_seconds: f64) {
        self.platform.update_time(elapsed_seconds);
    }

    pub fn resize(&mut self, size: PhysicalSize<u32>) {
        // See: https://github.com/rust-windowing/winit/issues/208
        if size.width > 0 && size.height > 0 {
            self.surface_config.width = size.width;
            self.surface_config.height = size.height;
            self.surface.configure(&self.device, &self.surface_config);
        }
    }

    pub fn paint(&mut self, cpu: &Cpu) {
        use wgpu::SurfaceError;

        let data = emu::pixel_buf(cpu);
        write_to_texture(&self.queue, &self.texture, data, self.texture_size);

        let output_frame = match self.surface.get_current_texture() {
            Ok(frame) => frame,
            Err(SurfaceError::Outdated) => return, // Occurs on minimization on Windows
            Err(e) => {
                eprintln!("Dropped frame with error: {}", e);
                return;
            }
        };

        let output_view = output_frame
            .texture
            .create_view(&TextureViewDescriptor::default());

        // Begin to draw Egui components
        self.platform.begin_frame();
        self.draw_egui(cpu, &self.platform.context(), self.texture_id);

        // End the UI frame. We could now handle the output and draw the UI with the backend.
        let full_output = self.platform.end_frame(Some(&self.window));
        let paint_jobs = self.platform.context().tessellate(full_output.shapes);

        let mut encoder = create_command_encoder(&self.device);
        let screen_descriptor = create_screen_descriptor(&self.window, &self.surface_config);
        let tdelta = full_output.textures_delta;
        // Upload all resources for the GPU.
        self.render_pass
            .add_textures(&self.device, &self.queue, &tdelta)
            .expect("add texture ok");
        self.render_pass
            .update_buffers(&self.device, &self.queue, &paint_jobs, &screen_descriptor);

        // Record all render passes.
        self.render_pass
            .execute(
                &mut encoder,
                &output_view,
                &paint_jobs,
                &screen_descriptor,
                Some(Color::BLACK),
            )
            .expect("execute render pass");

        // Submit the commands.
        self.queue.submit(std::iter::once(encoder.finish()));

        // Redraw egui
        output_frame.present();

        self.render_pass
            .remove_textures(tdelta)
            .expect("remove texture delta");
    }

    #[inline]
    pub fn draw_egui(&mut self, cpu: &Cpu, ctx: &Context, texture_id: TextureId) {
        use crate::{cpu, instruction, ppu};

        fn selectable_text(ui: &mut egui::Ui, mut text: &str) -> egui::Response {
            ui.add(egui::TextEdit::multiline(&mut text).code_editor())
        }

        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            ui.menu_button("File", |ui| {
                if ui.button("Quit").clicked() {
                    self.should_quit = true;
                }
            });

            egui::Window::new(&self.title).show(ctx, |ui| {
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
                    .selected_text(format!("{:?}", self.mode))
                    .show_ui(ui, |ui| {
                        ui.selectable_value(&mut self.mode, EmuMode::Running, "Running");
                        ui.selectable_value(&mut self.mode, EmuMode::Stopped, "Stopped");
                        ui.selectable_value(&mut self.mode, EmuMode::StepFrame, "Step Frame");
                        ui.selectable_value(&mut self.mode, EmuMode::Step, "Step");
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

                    // TODO: Reimplement this
                    // let r_len = ctx.fonts().glyph_width(egui::TextStyle::Body, 'R');
                    // let e_len = ctx.fonts().glyph_width(egui::TextStyle::Body, 'E');
                    // let q_len = ctx.fonts().glyph_width(egui::TextStyle::Body, 'Q');

                    ui.label("IE:");
                    // ui.add_space(q_len - (e_len - r_len));
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

fn build_window<T>(event_loop: &EventLoop<T>) -> Result<Window, OsError> {
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

fn request_adapter(instance: &Instance, surface: &Surface) -> Option<Adapter> {
    use wgpu::{PowerPreference, RequestAdapterOptions};

    pollster::block_on(instance.request_adapter(&RequestAdapterOptions {
        power_preference: PowerPreference::HighPerformance,
        compatible_surface: Some(surface),
        force_fallback_adapter: false, // TODO: What do I want to do with this?
    }))
}

fn request_device(adapter: &Adapter) -> Result<(Device, Queue), RequestDeviceError> {
    use wgpu::{DeviceDescriptor, Features, Limits};

    pollster::block_on(adapter.request_device(
        &DeviceDescriptor {
            features: Features::default(),
            limits: Limits::default(),
            label: None,
        },
        None,
    ))
}

fn surface_config(window: &Window, alpha_mode: CompositeAlphaMode,  format: TextureFormat) -> SurfaceConfiguration {
    use wgpu::PresentMode;

    let size = window.inner_size();
    SurfaceConfiguration {
        usage: TextureUsages::RENDER_ATTACHMENT,
        format,
        width: size.width as u32,
        height: size.height as u32,
        present_mode: PresentMode::Immediate,
        alpha_mode
    }
}

fn platform(window: &Window) -> Platform {
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

fn texture_size() -> Extent3d {
    Extent3d {
        width: GB_WIDTH as u32,
        height: GB_HEIGHT as u32,
        ..Default::default()
    }
}

fn create_texture(device: &Device, size: Extent3d) -> Texture {
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
fn write_to_texture(
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

fn expose_texture_to_egui(
    render_pass: &mut RenderPass,
    device: &Device,
    view: &TextureView,
) -> TextureId {
    render_pass.egui_texture_from_wgpu_texture(device, view, FILTER_MODE)
}

#[inline]
fn create_command_encoder(device: &Device) -> CommandEncoder {
    use wgpu::CommandEncoderDescriptor;

    device.create_command_encoder(&CommandEncoderDescriptor {
        label: Some("encoder"),
    })
}

#[inline]
fn create_screen_descriptor(window: &Window, config: &SurfaceConfiguration) -> ScreenDescriptor {
    ScreenDescriptor {
        physical_width: config.width,
        physical_height: config.height,
        scale_factor: window.scale_factor() as f32,
    }
}

pub mod kbd {
    use winit::event::{ElementState, KeyboardInput, VirtualKeyCode};

    pub fn space_released(input: &KeyboardInput) -> bool {
        let keycode = input.virtual_keycode;
        matches!(input.state, ElementState::Released if keycode == Some(VirtualKeyCode::Space))
    }
}
