use crate::cpu::Register;
use crate::cpu::RegisterPair;
use crate::LR35902;
use egui::{ClippedMesh, FontDefinitions};
use egui_wgpu_backend::{RenderPass, ScreenDescriptor};
use egui_winit_platform::{Platform, PlatformDescriptor};
use pixels::{wgpu, PixelsContext};
use std::time::Instant;
use wgpu::TextureFormat::Bgra8UnormSrgb;

// Boilerplate code from: https://github.com/parasyte/pixels/blob/0.3.0/examples/egui-winit/src/gui.rs

/// Manages all state required for rendering egui over `Pixels`.
pub struct Egui {
    // State for egui.
    start_time: Instant,
    platform: Platform,
    screen_desc: ScreenDescriptor,
    render_pass: RenderPass,
    paint_jobs: Vec<ClippedMesh>,

    pub config: Configuration,

    show_flags: bool,
    show_cpu_info: bool,
    show_registers: bool,

    #[cfg(feature = "debug")]
    show_disasm: bool,
    #[cfg(feature = "debug")]
    pub break_point: Option<u16>,
}

impl Egui {
    /// Create egui.
    pub fn new(width: u32, height: u32, scale_factor: f64, context: &PixelsContext) -> Self {
        let platform = Platform::new(PlatformDescriptor {
            physical_width: width,
            physical_height: height,
            scale_factor,
            font_definitions: FontDefinitions::default(),
            style: Default::default(),
        });

        let screen_desc = ScreenDescriptor {
            physical_width: width,
            physical_height: height,
            scale_factor: scale_factor as f32,
        };

        let render_pass = RenderPass::new(&context.device, Bgra8UnormSrgb);

        Self {
            start_time: Instant::now(),
            platform,
            screen_desc,
            render_pass,
            paint_jobs: Vec::new(),
            config: Default::default(),
            show_flags: false,
            show_cpu_info: false,
            show_registers: false,
            #[cfg(feature = "debug")]
            show_disasm: false,
            #[cfg(feature = "debug")]
            break_point: None,
        }
    }

    /// Handle input events from the window manager.
    pub fn handle_event(&mut self, event: &winit::event::Event<'_, ()>) {
        self.platform.handle_event(event);
    }

    /// Resize egui.
    pub fn resize(&mut self, width: u32, height: u32) {
        self.screen_desc.physical_width = width;
        self.screen_desc.physical_height = height;
    }

    /// Update scaling factor.
    pub fn scale_factor(&mut self, scale_factor: f64) {
        self.screen_desc.scale_factor = scale_factor as f32;
    }

    /// Prepare egui.
    pub fn prepare(&mut self, game_boy: &LR35902) {
        self.platform
            .update_time(self.start_time.elapsed().as_secs_f64());

        // Begin the egui frame.
        self.platform.begin_frame();

        // Draw the demo application.
        self.ui(&self.platform.context(), game_boy);

        // End the egui frame and create all paint jobs to prepare for rendering.
        let (_output, paint_commands) = self.platform.end_frame();
        self.paint_jobs = self.platform.context().tessellate(paint_commands);
    }

    /// Create the UI using egui.
    fn ui(&mut self, ctx: &egui::CtxRef, game_boy: &LR35902) {
        egui::TopPanel::top("menubar_container").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                egui::menu::menu(ui, "File", |ui| {
                    if ui.button("Configuration").clicked() {
                        self.config.show = true;
                    }
                });

                egui::menu::menu(ui, "Status", |ui| {
                    if ui.button("Flags").clicked() {
                        self.show_flags = true;
                    }

                    if ui.button("CPU Information").clicked() {
                        self.show_cpu_info = true;
                    }

                    if ui.button("Registers").clicked() {
                        self.show_registers = true;
                    }
                });

                #[cfg(feature = "debug")]
                if ui.button("Disassembly").clicked() {
                    self.show_disasm = true;
                }
            });
        });

        egui::Window::new("Cpu Flags")
            .open(&mut self.show_flags)
            .show(ctx, |ui| {
                let flags = game_boy.flags();

                ui.horizontal(|ui| {
                    let _ = ui.selectable_label(flags.z(), "Zero");
                    let _ = ui.selectable_label(flags.n(), "Negative");
                    let _ = ui.selectable_label(flags.h(), "Half-Carry");
                    let _ = ui.selectable_label(flags.c(), "Carry");
                });
            });

        egui::Window::new("Registers")
            .open(&mut self.show_registers)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label("A");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::A)));

                    ui.label("F");
                    let flag: u8 = game_boy.register(Register::Flag).into();
                    ui.monospace(format!("{:#04X}", flag));
                });

                ui.horizontal(|ui| {
                    ui.label("B");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::B)));

                    ui.label("C");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::C)));
                });

                ui.horizontal(|ui| {
                    ui.label("D");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::D)));

                    ui.label("E");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::E)));
                });

                ui.horizontal(|ui| {
                    ui.label("H");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::H)));

                    ui.label("L");
                    ui.monospace(format!("{:#04X}", game_boy.register(Register::L)));
                });

                ui.horizontal(|ui| {
                    ui.label("AF");
                    ui.monospace(format!("{:#06X}", game_boy.register_pair(RegisterPair::AF)));
                });

                ui.horizontal(|ui| {
                    ui.label("BC");
                    ui.monospace(format!("{:#06X}", game_boy.register_pair(RegisterPair::BC)));
                });

                ui.horizontal(|ui| {
                    ui.label("DE");
                    ui.monospace(format!("{:#06X}", game_boy.register_pair(RegisterPair::DE)));
                });

                ui.horizontal(|ui| {
                    ui.label("HL");
                    ui.monospace(format!("{:#06X}", game_boy.register_pair(RegisterPair::HL)));
                });
            });

        egui::Window::new("Cpu Information")
            .open(&mut self.show_cpu_info)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label("PC");
                    ui.monospace(format!("{:#06X}", game_boy.register_pair(RegisterPair::PC)));
                });

                ui.horizontal(|ui| {
                    ui.label("SP");
                    ui.monospace(format!("{:#06X}", game_boy.register_pair(RegisterPair::SP)));
                });

                ui.horizontal(|ui| {
                    ui.label("IME");
                    ui.label(format!("{:?}", game_boy.ime()));
                });

                ui.horizontal(|ui| {
                    ui.label("HALT");
                    ui.label(format!("{:?}", game_boy.halted()));
                });
            });

        #[cfg(feature = "debug")]
        {
            let mut show_disasm = self.show_disasm;
            egui::Window::new("Disassembler")
                .open(&mut show_disasm)
                .show(ctx, |ui| {
                    // FIXME: This can't be efficient at all
                    // To fix this, maybe we should make Instruction::from_byte not mutate the state of the Cpu (which we SHOULD have done to begin with)
                    let mut emu_clone = game_boy.clone();

                    for i in 0..10 {
                        let pc = emu_clone.register_pair(RegisterPair::PC);
                        let opcode = emu_clone.fetch();
                        emu_clone.inc_pc();

                        let instr = emu_clone.decode(opcode);
                        let res = ui.selectable_label(i == 0, format!("{:#06X}: {:?}", pc, instr));

                        if res.clicked() {
                            self.break_point = Some(pc);
                        }
                    }

                    ui.horizontal(|ui| {
                        if ui.button("Reset Breakpoint").clicked() {
                            self.break_point = None;
                        }

                        ui.selectable_label(
                            self.break_point.is_some(),
                            format!("{:04X?}", self.break_point),
                        )
                    });
                });
            self.show_disasm = show_disasm;
        }

        egui::Window::new("IRQ Information").show(ctx, |ui| {
            let req = game_boy.read_byte(0xFF0F);
            let enabled = game_boy.read_byte(0xFFFF);

            ui.heading("Interrupt Requests");
            ui.horizontal(|ui| {
                let _ = ui.selectable_label(req & 0x01 == 0x01, "VBLANK");
                let _ = ui.selectable_label((req >> 1) & 0x01 == 0x01, "LCD STAT");
                let _ = ui.selectable_label((req >> 2) & 0x01 == 0x01, "TIMER");
                let _ = ui.selectable_label((req >> 3) & 0x01 == 0x01, "SERIAL");
                let _ = ui.selectable_label((req >> 4) & 0x01 == 0x01, "JOYPAD");
            });

            ui.heading("Interrupt Enable");
            ui.horizontal(|ui| {
                let _ = ui.selectable_label(enabled & 0x01 == 0x01, "VBLANK");
                let _ = ui.selectable_label((enabled >> 1) & 0x01 == 0x01, "LCD STAT");
                let _ = ui.selectable_label((enabled >> 2) & 0x01 == 0x01, "TIMER");
                let _ = ui.selectable_label((enabled >> 3) & 0x01 == 0x01, "SERIAL");
                let _ = ui.selectable_label((enabled >> 4) & 0x01 == 0x01, "JOYPAD");
            });
        });

        #[cfg(feature = "debug")]
        let mut spacebar_step = self.config.spacebar_step;
        egui::Window::new("Configuration")
            .open(&mut self.config.show)
            .show(ctx, |ui| {
                #[cfg(feature = "debug")]
                ui.horizontal(|ui| {
                    ui.label("Spacebar Steps");
                    ui.add(egui::Slider::u16(&mut spacebar_step, 0..=std::u16::MAX));
                });
            });

        #[cfg(feature = "debug")]
        {
            self.config.spacebar_step = spacebar_step;
        }
    }

    /// Render egui.
    pub fn render(
        &mut self,
        encoder: &mut wgpu::CommandEncoder,
        render_target: &wgpu::TextureView,
        context: &PixelsContext,
    ) {
        // Upload all resources to the GPU.
        self.render_pass.update_texture(
            &context.device,
            &context.queue,
            &self.platform.context().texture(),
        );
        self.render_pass
            .update_user_textures(&context.device, &context.queue);
        self.render_pass.update_buffers(
            &context.device,
            &context.queue,
            &self.paint_jobs,
            &self.screen_desc,
        );

        // Record all render passes.
        self.render_pass.execute(
            encoder,
            render_target,
            &self.paint_jobs,
            &self.screen_desc,
            None,
        );
    }
}

pub struct Configuration {
    /// Show Configuration egui menu
    show: bool,

    /// How many [`LR35902`] .step() do we want to do at once
    /// when pressing the spacebar key?
    #[cfg(feature = "debug")]
    pub spacebar_step: u16,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            show: false,
            #[cfg(feature = "debug")]
            spacebar_step: 1,
        }
    }
}
