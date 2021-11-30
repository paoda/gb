use crate::apu::gen::SAMPLE_RATE;
use crate::bus::BusIo;
use crate::emu::SM83_CLOCK_SPEED;
use gen::SampleProducer;
use types::ch1::{Sweep, SweepDirection};
use types::ch3::Volume as Ch3Volume;
use types::ch4::{CounterWidth, Frequency as Ch4Frequency, PolynomialCounter};
use types::common::{EnvelopeDirection, FrequencyHigh, SoundDuty, VolumeEnvelope};
use types::fs::{FrameSequencer, State as FSState};
use types::{ChannelControl, NRx4, SoundOutput};

pub mod gen;
mod types;

const SAMPLE_INCREMENT: u64 = gen::SAMPLE_RATE as u64;
const WAVE_PATTERN_RAM_LEN: usize = 0x10;

#[derive(Default, Debug)]
pub struct Apu {
    ctrl: SoundControl,
    /// Tone & Sweep
    ch1: Channel1,
    /// Tone
    ch2: Channel2,
    /// Wave
    ch3: Channel3,
    /// Noise
    ch4: Channel4,

    fs: FrameSequencer,
    div_prev: Option<u16>,

    pub(crate) prod: Option<SampleProducer<f32>>,
    sample_counter: u64,

    cap: f32,
}

impl BusIo for Apu {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr & 0x00FF {
            0x10 => self.ch1.sweep(),
            0x11 => self.ch1.duty(),
            0x12 => self.ch1.envelope(),
            0x14 => self.ch1.freq_hi(),
            0x16 => self.ch2.duty(),
            0x17 => self.ch2.envelope(),
            0x19 => self.ch2.freq_hi(),
            0x1A => self.ch3.dac_enabled(),
            0x1C => self.ch3.volume(),
            0x1E => self.ch3.freq_hi(),
            0x21 => self.ch4.envelope(),
            0x22 => self.ch4.poly(),
            0x23 => self.ch4.frequency(),
            0x24 => self.ctrl.channel(),
            0x25 => self.ctrl.output(),
            0x26 => self.ctrl.status(self),
            0x30..=0x3F => self.ch3.read_byte(addr),
            _ => {
                tracing::warn!("Attempted read from {:#06X}", addr);
                0xFF
            }
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr & 0x00FF {
            0x10 if self.ctrl.enabled => self.ch1.set_sweep(byte),
            0x11 if self.ctrl.enabled => self.ch1.set_duty(byte),
            0x12 if self.ctrl.enabled => self.ch1.set_envelope(byte),
            0x13 if self.ctrl.enabled => self.ch1.set_freq_lo(byte),
            0x14 if self.ctrl.enabled => self.ch1.set_freq_hi(&self.fs, byte),
            0x16 if self.ctrl.enabled => self.ch2.set_duty(byte),
            0x17 if self.ctrl.enabled => self.ch2.set_envelope(byte),
            0x18 if self.ctrl.enabled => self.ch2.set_freq_lo(byte),
            0x19 if self.ctrl.enabled => self.ch2.set_freq_hi(&self.fs, byte),
            0x1A if self.ctrl.enabled => self.ch3.set_dac_enabled(byte),
            0x1B if self.ctrl.enabled => self.ch3.set_len(byte),
            0x1C if self.ctrl.enabled => self.ch3.set_volume(byte),
            0x1D if self.ctrl.enabled => self.ch3.set_freq_lo(byte),
            0x1E if self.ctrl.enabled => self.ch3.set_freq_hi(&self.fs, byte),
            0x20 if self.ctrl.enabled => self.ch4.set_len(byte),
            0x21 if self.ctrl.enabled => self.ch4.set_envelope(byte),
            0x22 if self.ctrl.enabled => self.ch4.set_poly(byte),
            0x23 if self.ctrl.enabled => self.ch4.set_frequency(&self.fs, byte),
            0x24 if self.ctrl.enabled => self.ctrl.set_channel(byte),
            0x25 if self.ctrl.enabled => self.ctrl.set_output(byte),
            0x26 => self.set_status(byte),
            0x30..=0x3F => self.ch3.write_byte(addr, byte),
            _ if !self.ctrl.enabled => {}
            _ => tracing::warn!("Attempted write of {:#04X} to {:#06X}", byte, addr),
        }
    }
}

impl Apu {
    pub(crate) fn tick(&mut self, div: u16) {
        self.sample_counter += SAMPLE_INCREMENT;

        // Frame Sequencer (512Hz)
        if self.is_falling_edge(12, div) {
            use FSState::*;

            match self.fs.state() {
                Length => self.clock_length(),
                LengthAndSweep => {
                    self.clock_length();
                    self.clock_sweep();
                }
                Envelope => self.clock_envelope(),
                Nothing => {}
            }

            self.fs.next();
        }

        self.div_prev = Some(div);

        self.ch1.tick();
        self.ch2.tick();
        self.ch3.tick();
        self.ch4.tick();

        if self.sample_counter >= SM83_CLOCK_SPEED {
            self.sample_counter %= SM83_CLOCK_SPEED;

            if let Some(prod) = self.prod.as_mut() {
                if prod.available_blocking() {
                    // Sample the APU

                    let ch1_amplitude =
                        Self::high_pass(&mut self.cap, self.ch1.amplitude(), self.ch1.enabled);
                    let (left, right) = self.ctrl.out.ch1();
                    let ch1_left = if left { ch1_amplitude } else { 0.0 };
                    let ch1_right = if right { ch1_amplitude } else { 0.0 };

                    let ch2_amplitude =
                        Self::high_pass(&mut self.cap, self.ch2.amplitude(), self.ch2.enabled);
                    let (left, right) = self.ctrl.out.ch2();
                    let ch2_left = if left { ch2_amplitude } else { 0.0 };
                    let ch2_right = if right { ch2_amplitude } else { 0.0 };

                    let ch3_amplitude =
                        Self::high_pass(&mut self.cap, self.ch3.amplitude(), self.ch3.enabled);
                    let (left, right) = self.ctrl.out.ch3();
                    let ch3_left = if left { ch3_amplitude } else { 0.0 };
                    let ch3_right = if right { ch3_amplitude } else { 0.0 };

                    let ch4_amplitude =
                        Self::high_pass(&mut self.cap, self.ch4.amplitude(), self.ch4.enabled);
                    let (left, right) = self.ctrl.out.ch4();
                    let ch4_left = if left { ch4_amplitude } else { 0.0 };
                    let ch4_right = if right { ch4_amplitude } else { 0.0 };

                    let left_mixed = (ch1_left + ch2_left + ch3_left + ch4_left) / 4.0;
                    let right_mixed = (ch1_right + ch2_right + ch3_right + ch4_right) / 4.0;

                    let left_sample = (self.ctrl.channel.left_volume() + 1.0) * left_mixed;
                    let right_sample = (self.ctrl.channel.right_volume() + 1.0) * right_mixed;

                    prod.push(left_sample)
                        .and(prod.push(right_sample))
                        .expect("Add samples to ring buffer");
                }
            }
        }
    }

    /// 0xFF26 | NR52 - Sound On/Off
    pub(crate) fn set_status(&mut self, byte: u8) {
        self.ctrl.enabled = (byte >> 7) & 0x01 == 0x01;

        if self.ctrl.enabled {
            // Frame Sequencer reset to Step 0
            self.fs.reset();

            // Square Duty units are reset to first step
            self.ch1.duty_pos = 0;
            self.ch2.duty_pos = 0;

            // Wave Channel's sample buffer reset to 0
            self.ch3.offset = 0;
        }

        if !self.ctrl.enabled {
            self.reset();
        } else {
        }
    }

    fn reset(&mut self) {
        self.ch1.sweep = Default::default();
        self.ch1.duty = Default::default();
        self.ch1.envelope = Default::default();
        self.ch1.freq_lo = Default::default();
        self.ch1.freq_hi = Default::default();

        self.ch2.duty = Default::default();
        self.ch2.envelope = Default::default();
        self.ch2.freq_lo = Default::default();
        self.ch2.freq_hi = Default::default();

        self.ch3.dac_enabled = Default::default();
        self.ch3.len = Default::default();
        self.ch3.volume = Default::default();
        self.ch3.freq_lo = Default::default();
        self.ch3.freq_hi = Default::default();

        self.ch4.len = Default::default();
        self.ch4.envelope = Default::default();
        self.ch4.poly = Default::default();
        self.ch4.freq = Default::default();

        self.ctrl.channel = ChannelControl(0);
        self.ctrl.out = SoundOutput(0);

        // Disable the Channels
        self.ch1.enabled = Default::default();
        self.ch2.enabled = Default::default();
        self.ch3.enabled = Default::default();
        self.ch4.enabled = Default::default();
    }

    fn process_length(freq: &impl NRx4, counter: &mut u16, enabled: &mut bool) {
        if freq.length_enable() && *counter > 0 {
            *counter -= 1;

            // Check in this scope ensures (only) the above subtraction
            // made length_timer 0
            if *counter == 0 {
                *enabled = false;
            }
        }
    }

    fn clock_length(&mut self) {
        Self::process_length(
            &self.ch1.freq_hi,
            &mut self.ch1.length_counter,
            &mut self.ch1.enabled,
        );

        Self::process_length(
            &self.ch2.freq_hi,
            &mut self.ch2.length_counter,
            &mut self.ch2.enabled,
        );

        Self::process_length(
            &self.ch3.freq_hi,
            &mut self.ch3.length_counter,
            &mut self.ch3.enabled,
        );

        Self::process_length(
            &self.ch4.freq,
            &mut self.ch4.length_counter,
            &mut self.ch4.enabled,
        );
    }

    fn clock_sweep(&mut self) {
        if self.ch1.sweep_timer != 0 {
            self.ch1.sweep_timer -= 1;
        }

        if self.ch1.sweep_timer == 0 {
            let period = self.ch1.sweep.period();
            self.ch1.sweep_timer = if period == 0 { 8 } else { period };

            if self.ch1.sweep_enabled && period != 0 {
                let new_freq = self.ch1.calc_sweep_freq();

                if new_freq <= 0x7FF && self.ch1.sweep.shift_count() != 0 {
                    self.ch1.set_frequency(new_freq);
                    self.ch1.shadow_freq = new_freq;

                    let _ = self.ch1.calc_sweep_freq();
                }
            }
        }
    }

    fn process_envelope(envelope: &VolumeEnvelope, period_timer: &mut u8, current_volume: &mut u8) {
        use EnvelopeDirection::*;

        if envelope.period() != 0 {
            if *period_timer != 0 {
                *period_timer -= 1;
            }

            if *period_timer == 0 {
                *period_timer = envelope.period();

                match envelope.direction() {
                    Decrease if *current_volume > 0x00 => *current_volume -= 1,
                    Increase if *current_volume < 0x0F => *current_volume += 1,
                    _ => {}
                }
            }
        }
    }

    fn clock_envelope(&mut self) {
        // Channels 1, 2 and 4 have Volume Envelopes

        Self::process_envelope(
            &self.ch1.envelope,
            &mut self.ch1.period_timer,
            &mut self.ch1.current_volume,
        );

        Self::process_envelope(
            &self.ch2.envelope,
            &mut self.ch2.period_timer,
            &mut self.ch2.current_volume,
        );

        Self::process_envelope(
            &self.ch4.envelope,
            &mut self.ch4.period_timer,
            &mut self.ch4.current_volume,
        );
    }

    fn is_falling_edge(&self, bit: u8, div: u16) -> bool {
        match self.div_prev {
            Some(p) => (p >> bit & 0x01) == 0x01 && (div >> bit & 0x01) == 0x00,
            None => false,
        }
    }

    fn high_pass(capacitor: &mut f32, input: f32, enabled: bool) -> f32 {
        const CHARGE_FACTOR: f32 = 0.999958;

        let mut output = 0.0;
        if enabled {
            output = input - *capacitor;
            *capacitor =
                input - output * CHARGE_FACTOR.powi(SM83_CLOCK_SPEED as i32 / SAMPLE_RATE as i32);
        }

        output
    }
}

#[derive(Debug)]
pub(crate) struct SoundControl {
    /// 0xFF24 | NR50 - Channel Control
    channel: ChannelControl,
    /// 0xFF25 | NR51 - Selection of Sound output terminal
    out: SoundOutput,

    enabled: bool,
}

impl Default for SoundControl {
    fn default() -> Self {
        Self {
            channel: ChannelControl(0),
            out: SoundOutput(0),
            enabled: Default::default(),
        }
    }
}

impl SoundControl {
    /// 0xFF24 | NR50 - Channel Control
    pub(crate) fn channel(&self) -> u8 {
        u8::from(self.channel)
    }

    /// 0xFF24 | NR50 - Channel Control
    pub(crate) fn set_channel(&mut self, byte: u8) {
        if self.enabled {
            self.channel = byte.into();
        }
    }

    /// 0xFF25 | NR51 - Selection of Sound output terminal
    pub(crate) fn output(&self) -> u8 {
        u8::from(self.out)
    }

    /// 0xFF25 | NR51 - Selection of Sound output terminal
    pub(crate) fn set_output(&mut self, byte: u8) {
        if self.enabled {
            self.out = byte.into();
        }
    }

    /// 0xFF26 | NR52 - Sound On/Off
    pub(crate) fn status(&self, apu: &Apu) -> u8 {
        (self.enabled as u8) << 7
            | (apu.ch4.enabled as u8) << 3
            | (apu.ch3.enabled as u8) << 2
            | (apu.ch2.enabled as u8) << 1
            | apu.ch1.enabled as u8
            | 0x70
    }
}

#[derive(Debug, Default)]
pub(crate) struct Channel1 {
    /// 0xFF10 | NR10 - Channel 1 Sweep Register
    sweep: Sweep,
    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    duty: SoundDuty,
    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    envelope: VolumeEnvelope,
    /// 0xFF13 | NR13 - Channel 1 Frequency low (lower 8 bits only)
    freq_lo: u8,
    /// 0xFF14 | NR14 - Channel 1 Frequency high
    freq_hi: FrequencyHigh,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,

    // Sweep Functionality
    sweep_timer: u8,
    shadow_freq: u16,
    sweep_enabled: bool,

    // Length Functionality
    length_counter: u16,

    freq_timer: u16,
    duty_pos: u8,

    enabled: bool,
}

impl Channel1 {
    /// 0xFF10 | NR10 - Channel 1 Sweep Register
    pub(crate) fn sweep(&self) -> u8 {
        u8::from(self.sweep) | 0x80
    }

    /// 0xFF10 | NR10 - Channel 1 Sweep Register
    pub(crate) fn set_sweep(&mut self, byte: u8) {
        self.sweep = byte.into()
    }

    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    pub(crate) fn duty(&self) -> u8 {
        u8::from(self.duty) | 0x3F
    }

    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    pub(crate) fn set_duty(&mut self, byte: u8) {
        self.duty = byte.into();
        self.length_counter = 64 - self.duty.sound_length() as u16;
    }

    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    pub(crate) fn envelope(&self) -> u8 {
        u8::from(self.envelope)
    }

    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    pub(crate) fn set_envelope(&mut self, byte: u8) {
        self.envelope = byte.into();

        if !self.is_dac_enabled() {
            self.enabled = false;
        }
    }

    /// 0xFF13 | NR13 - Channel 1 Frequency low (lower 8 bits only)
    pub(crate) fn set_freq_lo(&mut self, byte: u8) {
        self.freq_lo = byte;
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        u8::from(self.freq_hi) | 0xBF
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn set_freq_hi(&mut self, fs: &FrameSequencer, byte: u8) {
        let mut new_freq: FrequencyHigh = byte.into();

        // If this bit is set, a trigger event occurs
        if new_freq.trigger() {
            self.enabled = true;

            if self.length_counter == 0 {
                self.length_counter = 64;

                // length_counter was 0 so length evidently wasn't enabled
                self.freq_hi.set_length_enable(false);
            };

            // reload freq_timer but last two bits are unmodified
            self.freq_timer = obscure::square::freq_timer_reload(self.freq_timer, self.frequency());

            // Volume Envelope loaded w/ period
            self.period_timer = self.envelope.period();

            // Channel Volume reloaded
            self.current_volume = self.envelope.init_vol();

            // Channel 1 Sweep Behaviour
            let sweep_period = self.sweep.period();
            let sweep_shift = self.sweep.shift_count();

            self.shadow_freq = self.frequency();
            self.sweep_timer = if sweep_period == 0 { 8 } else { sweep_period };

            self.sweep_enabled = sweep_period != 0 || sweep_shift != 0;

            if sweep_shift != 0 {
                let _ = self.calc_sweep_freq();
            }

            self.enabled = self.is_dac_enabled();
        }

        obscure::nrx4::length_update(
            &mut new_freq,
            fs,
            &mut self.length_counter,
            &mut self.enabled,
            self.freq_hi.length_enable(),
        );

        self.freq_hi = new_freq;
    }

    fn tick(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            self.freq_timer = (2048 - self.frequency()) * 4;
            self.duty_pos = (self.duty_pos + 1) % 8;
        }
    }

    fn amplitude(&self) -> f32 {
        if self.is_dac_enabled() {
            let sample = self.duty.wave_pattern().amplitude(self.duty_pos) * self.current_volume;
            let input = if self.enabled { sample } else { 0 };

            (input as f32 / 7.5) - 1.0
        } else {
            0.0
        }
    }

    fn calc_sweep_freq(&mut self) -> u16 {
        use SweepDirection::*;

        let shadow_freq_shifted = self.shadow_freq >> self.sweep.shift_count();
        let new_freq = match self.sweep.direction() {
            Increase => self.shadow_freq + shadow_freq_shifted,
            Decrease => self.shadow_freq - shadow_freq_shifted,
        };

        // Overflow check
        if new_freq > 2047 {
            self.enabled = false;
        }

        new_freq
    }

    fn set_frequency(&mut self, word: u16) {
        let freq_bits = word & 0x07FF;
        self.freq_lo = (freq_bits & 0x00FF) as u8;
        self.freq_hi
            .set_freq_bits(((freq_bits & 0x0700) >> 8) as u8);
    }

    fn frequency(&self) -> u16 {
        (self.freq_hi.freq_bits() as u16) << 8 | self.freq_lo as u16
    }

    fn is_dac_enabled(&self) -> bool {
        self.envelope.0 & 0xF8 != 0x00
    }
}

#[derive(Debug, Default)]
pub(crate) struct Channel2 {
    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    duty: SoundDuty,
    /// 0xFF17 | NR22 - Channel 2 Volume Envelope
    envelope: VolumeEnvelope,
    /// 0xFF18 | NR23 - Channel 2 Frequency low (lower 8 bits only)
    freq_lo: u8,
    /// 0xFF19 | NR24 - Channel 2 Frequency high
    freq_hi: FrequencyHigh,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,

    // Length Functionality
    length_counter: u16,

    freq_timer: u16,
    duty_pos: u8,

    enabled: bool,
}

impl Channel2 {
    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) fn duty(&self) -> u8 {
        u8::from(self.duty) | 0x3F
    }

    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) fn set_duty(&mut self, byte: u8) {
        self.duty = byte.into();
        self.length_counter = 64 - self.duty.sound_length() as u16;
    }

    /// 0xFF17 | NR22 - Channel 2 Volume Envelope
    pub(crate) fn envelope(&self) -> u8 {
        u8::from(self.envelope)
    }

    /// 0xFF17 | NR22 - Channel 2 Volume Envelope
    pub(crate) fn set_envelope(&mut self, byte: u8) {
        self.envelope = byte.into();

        if !self.is_dac_enabled() {
            self.enabled = false;
        }
    }

    /// 0xFF18 | NR23 - Channel 2 Frequency low (lower 8 bits only)
    pub(crate) fn set_freq_lo(&mut self, byte: u8) {
        self.freq_lo = byte;
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        u8::from(self.freq_hi) | 0xBF
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn set_freq_hi(&mut self, fs: &FrameSequencer, byte: u8) {
        let prev_le = self.freq_hi.length_enable();
        self.freq_hi = byte.into();

        obscure::nrx4::length_update(
            &mut self.freq_hi,
            fs,
            &mut self.length_counter,
            &mut self.enabled,
            prev_le,
        );

        if self.freq_hi.trigger() {
            self.enabled = true;

            // Reload length counter if need be
            if self.length_counter == 0 {
                self.length_counter = 64;
            }

            // reload frequency timer
            self.freq_timer = obscure::square::freq_timer_reload(self.freq_timer, self.frequency());

            // reload envelope
            self.period_timer = self.envelope.period();

            // reload volume
            self.current_volume = self.envelope.init_vol();

            self.enabled = self.is_dac_enabled();
        }
    }

    fn amplitude(&self) -> f32 {
        if self.is_dac_enabled() {
            let sample = self.duty.wave_pattern().amplitude(self.duty_pos) * self.current_volume;
            let input = if self.enabled { sample } else { 0 };

            (input as f32 / 7.5) - 1.0
        } else {
            0.0
        }
    }

    fn tick(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            self.freq_timer = (2048 - self.frequency()) * 4;
            self.duty_pos = (self.duty_pos + 1) % 8;
        }
    }

    fn frequency(&self) -> u16 {
        (self.freq_hi.freq_bits() as u16) << 8 | self.freq_lo as u16
    }

    fn is_dac_enabled(&self) -> bool {
        self.envelope.0 & 0xF8 != 0x00
    }
}

#[derive(Debug, Default)]
pub(crate) struct Channel3 {
    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    dac_enabled: bool,
    /// 0xFF1B | NR31 - Sound Length
    len: u8,
    /// 0xFF1C | NR32 - Channel 3 Volume
    volume: Ch3Volume,
    /// 0xFF1D | NR33 - Channel 3 Frequency low (lower 8 bits)
    freq_lo: u8,
    /// 0xFF1E | NR34 - Channel 3 Frequency high
    freq_hi: FrequencyHigh,

    wave_ram: [u8; WAVE_PATTERN_RAM_LEN],

    // Length Functionality
    length_counter: u16,

    freq_timer: u16,
    offset: u8,

    enabled: bool,
}

impl BusIo for Channel3 {
    fn read_byte(&self, addr: u16) -> u8 {
        if self.enabled {
            self.wave_ram[self.offset as usize / 2]
        } else {
            self.wave_ram[(addr - Self::WAVE_RAM_START_ADDR) as usize]
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        if self.enabled {
            self.wave_ram[self.offset as usize / 2] = byte;
        } else {
            self.wave_ram[(addr - Self::WAVE_RAM_START_ADDR) as usize] = byte;
        }
    }
}

impl Channel3 {
    const WAVE_RAM_START_ADDR: u16 = 0xFF30;

    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    pub(crate) fn dac_enabled(&self) -> u8 {
        ((self.dac_enabled as u8) << 7) | 0x7F
    }

    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    pub(crate) fn set_dac_enabled(&mut self, byte: u8) {
        self.dac_enabled = (byte >> 7) & 0x01 == 0x01;

        if !self.dac_enabled {
            self.enabled = false;
        }
    }

    /// 0xFF1B | NR31 - Sound Length
    pub(crate) fn set_len(&mut self, byte: u8) {
        self.len = byte;
        self.length_counter = 256 - self.len as u16;
    }

    /// 0xFF1C | NR32 - Channel 3 Volume
    pub(crate) fn volume(&self) -> u8 {
        ((self.volume as u8) << 5) | 0x9F
    }

    /// 0xFF1C | NR32 - Channel 3 Volume
    pub(crate) fn set_volume(&mut self, byte: u8) {
        use Ch3Volume::*;

        self.volume = match (byte >> 5) & 0x03 {
            0b00 => Mute,
            0b01 => Full,
            0b10 => Half,
            0b11 => Quarter,
            _ => unreachable!("{:#04X} is not a valid value for Channel3Volume", byte),
        };
    }

    /// 0xFF1D | NR33 - Channel 3 Frequency low (lower 8 bits)
    pub(crate) fn set_freq_lo(&mut self, byte: u8) {
        self.freq_lo = byte;
    }

    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        u8::from(self.freq_hi) | 0xBF
    }

    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) fn set_freq_hi(&mut self, fs: &FrameSequencer, byte: u8) {
        let prev_le = self.freq_hi.length_enable();
        self.freq_hi = byte.into();

        obscure::nrx4::length_update(
            &mut self.freq_hi,
            fs,
            &mut self.length_counter,
            &mut self.enabled,
            prev_le,
        );

        if self.freq_hi.trigger() {
            self.enabled = true;

            // Length behaviour during trigger event
            if self.length_counter == 0 {
                self.length_counter = 256;
            }

            self.freq_timer = (2048 - self.frequency()) * 2;

            // reset wave channel's ptr into wave RAM
            self.offset = 0;

            self.enabled = self.dac_enabled;
        }
    }

    fn tick(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            self.freq_timer = (2048 - self.frequency()) * 2;
            self.offset = (self.offset + 1) % (WAVE_PATTERN_RAM_LEN * 2) as u8;
        }
    }

    fn amplitude(&self) -> f32 {
        if self.dac_enabled {
            let sample = self.read_sample(self.offset) >> self.volume.shift_count();
            let input = if self.enabled { sample } else { 0 };

            (input as f32 / 7.5) - 1.0
        } else {
            0.0
        }
    }

    fn read_sample(&self, index: u8) -> u8 {
        let i = (index / 2) as usize;

        if index % 2 == 0 {
            // We grab the high nibble on even indexes
            self.wave_ram[i] >> 4
        } else {
            // We grab the low nibble on odd indexes
            self.wave_ram[i] & 0x0F
        }
    }

    fn frequency(&self) -> u16 {
        (self.freq_hi.freq_bits() as u16) << 8 | self.freq_lo as u16
    }
}

#[derive(Debug, Default)]
pub(crate) struct Channel4 {
    /// 0xFF20 | NR41 - Channel 4 Sound Length
    len: u8,
    /// 0xFF21 | NR42 - Channel 4 Volume Envelope
    envelope: VolumeEnvelope,
    /// 0xFF22 | NR43 - Chanel 4 Polynomial Counter
    poly: PolynomialCounter,
    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    freq: Ch4Frequency,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,

    // Length Functionality
    length_counter: u16,

    /// Linear Feedback Shift Register (15-bit)
    lf_shift: u16,

    freq_timer: u16,

    enabled: bool,
}

impl Channel4 {
    /// 0xFF20 | NR41 - Channel 4 Sound Length
    pub(crate) fn set_len(&mut self, byte: u8) {
        self.len = byte & 0x3F;
        self.length_counter = 64 - self.len as u16;
    }

    /// 0xFF21 | NR42 - Channel 4 Volume Envelope
    pub(crate) fn envelope(&self) -> u8 {
        u8::from(self.envelope)
    }

    /// 0xFF21 | NR42 - Channel 4 Volume Envelope
    pub(crate) fn set_envelope(&mut self, byte: u8) {
        self.envelope = byte.into();

        if !self.is_dac_enabled() {
            self.enabled = false;
        }
    }

    /// 0xFF22 | NR43 - Chanel 4 Polynomial Counter
    pub(crate) fn poly(&self) -> u8 {
        u8::from(self.poly)
    }

    /// 0xFF22 | NR43 - Chanel 4 Polynomial Counter
    pub(crate) fn set_poly(&mut self, byte: u8) {
        self.poly = byte.into();
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn frequency(&self) -> u8 {
        u8::from(self.freq) | 0xBF
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn set_frequency(&mut self, fs: &FrameSequencer, byte: u8) {
        let prev_le = self.freq.length_enable();
        self.freq = byte.into();

        obscure::nrx4::length_update(
            &mut self.freq,
            fs,
            &mut self.length_counter,
            &mut self.enabled,
            prev_le,
        );

        if self.freq.trigger() {
            self.enabled = true;

            if self.length_counter == 0 {
                self.length_counter = 64;
            }

            // FIXME: Frequency Timer reloaded?

            // reload envelope
            self.period_timer = self.envelope.period();

            //reload volume
            self.current_volume = self.envelope.init_vol();

            // LFSR reset
            self.lf_shift = 0x7FFF;

            self.enabled = self.is_dac_enabled();
        }
    }

    fn tick(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            let divisor = Self::divisor(self.poly.divisor_code()) as u16;
            self.freq_timer = divisor << self.poly.shift_count();

            let xor_result = (self.lf_shift & 0x01) ^ ((self.lf_shift & 0x02) >> 1);
            self.lf_shift = (self.lf_shift >> 1) | xor_result << 14;

            if let CounterWidth::Long = self.poly.counter_width() {
                self.lf_shift = (self.lf_shift & !(0x01 << 6)) | xor_result << 6;
            }
        }
    }

    fn amplitude(&self) -> f32 {
        if self.is_dac_enabled() {
            let sample = (!self.lf_shift & 0x01) as u8 * self.current_volume;
            let input = if self.enabled { sample } else { 0 };

            (input as f32 / 7.5) - 1.0
        } else {
            0.0
        }
    }

    fn is_dac_enabled(&self) -> bool {
        self.envelope.0 & 0xF8 != 0x00
    }

    fn divisor(code: u8) -> u8 {
        if code == 0 {
            return 8;
        }

        code << 4
    }
}

mod obscure {
    pub(super) mod square {
        pub(crate) fn freq_timer_reload(freq_timer: u16, frequency: u16) -> u16 {
            (freq_timer & 0x0003) | (((2048 - frequency) * 4) & 0xFFFC)
        }
    }

    pub(super) mod nrx4 {
        use super::super::{FrameSequencer, NRx4};

        /// Implements the obscure behaviour when writing to NRX4 under certain
        /// conditions
        ///
        /// # Arguments
        /// * `freq_hi` - mutable reference to a channel's frequency high register
        /// * `fs` - reference to the APU's frame sequencer
        /// * `counter` - mutable reference to a channel's internal enabled flag
        /// * `prev_le` - what length_enable was before NRx4 was written with a new value
        pub(crate) fn length_update(
            freq: &mut impl NRx4,
            fs: &FrameSequencer,
            counter: &mut u16,
            enabled: &mut bool,
            prev_le: bool,
        ) {
            if !fs.next_clocks_length() && !prev_le && freq.length_enable() && *counter != 0 {
                *counter -= 1;

                if *counter == 0 && !freq.trigger() {
                    *enabled = false;
                }
            }
        }
    }
}
