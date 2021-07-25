use crate::bus::BusIo;
use crate::emu::SM83_CLOCK_SPEED;
use gen::{AudioBuffer, AudioSender};
use types::ch1::{Sweep, SweepDirection};
use types::ch3::Volume as Ch3Volume;
use types::ch4::{CounterWidth, Frequency as Ch4Frequency, PolynomialCounter};
use types::common::{EnvelopeDirection, FrequencyHigh, SoundDuty, VolumeEnvelope};
use types::{ChannelControl, FrameSequencerState, SoundOutput};

pub mod gen;
mod types;

const WAVE_PATTERN_RAM_LEN: usize = 0x10;

const SAMPLE_RATE: u32 = 48000; // Hz
const AUDIO_BUFFER_LEN: usize = 512;
const CHANNEL_COUNT: usize = 2;
const SAMPLE_INCREMENT: u64 = SAMPLE_RATE as u64;

#[derive(Default, Debug, Clone)]
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

    // Frame Sequencer
    frame_seq_state: FrameSequencerState,
    div_prev: Option<u8>,

    sender: Option<AudioSender<f32>>,
    sample_counter: u64,

    buffer: AudioBuffer<(f32, f32)>,
}

impl BusIo for Apu {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr & 0x00FF {
            0x11 => self.ch1.duty(),
            0x12 => self.ch1.envelope(),
            0x14 => self.ch1.freq_hi(),
            0x16 => self.ch2.duty(),
            0x17 => self.ch2.envelope(),
            0x19 => self.ch2.freq_hi(),
            0x1A => self.ch3.enabled(),
            0x1B => self.ch3.len(),
            0x1C => self.ch3.volume(),
            0x1E => self.ch3.freq_hi(),
            0x20 => self.ch4.len(),
            0x21 => self.ch4.envelope(),
            0x22 => self.ch4.poly(),
            0x23 => self.ch4.frequency(),
            0x24 => self.ctrl.channel(),
            0x25 => self.ctrl.output(),
            0x26 => self.ctrl.status(self),
            0x30..=0x3F => self.ch3.read_byte(addr),
            _ => {
                eprintln!("Read 0xFF from unused IO register {:#06X} [APU]", addr);
                0xFF
            }
        }
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        match addr & 0x00FF {
            0x10 => self.ch1.set_sweep(byte),
            0x11 => self.ch1.set_duty(byte),
            0x12 => self.ch1.set_envelope(byte),
            0x13 => self.ch1.set_freq_lo(byte),
            0x14 => self.ch1.set_freq_hi(byte),
            0x16 => self.ch2.set_duty(byte),
            0x17 => self.ch2.set_envelope(byte),
            0x18 => self.ch2.set_freq_lo(byte),
            0x19 => self.ch2.set_freq_hi(byte),
            0x1A => self.ch3.set_enabled(byte),
            0x1B => self.ch3.set_len(byte),
            0x1C => self.ch3.set_volume(byte),
            0x1D => self.ch3.set_freq_lo(byte),
            0x1E => self.ch3.set_freq_hi(byte),
            0x20 => self.ch4.set_len(byte),
            0x21 => self.ch4.set_envelope(byte),
            0x22 => self.ch4.set_poly(byte),
            0x23 => self.ch4.set_freq_data(byte),
            0x24 => self.ctrl.set_channel(byte),
            0x25 => self.ctrl.set_output(byte),
            0x26 => self.set_status(byte),
            0x30..=0x3F => self.ch3.write_byte(addr, byte),
            _ => eprintln!(
                "Wrote {:#04X} to unused IO register {:#06X} [APU]",
                byte, addr
            ),
        }
    }
}

impl Apu {
    pub(crate) fn clock(&mut self, div: u16) {
        use FrameSequencerState::*;
        self.sample_counter += SAMPLE_INCREMENT;

        // the 5th bit of the high byte
        let bit_5 = (div >> 13 & 0x01) as u8;

        if let Some(0x01) = self.div_prev {
            if bit_5 == 0x00 {
                // Falling Edge, step the Frame Sequencer
                self.frame_seq_state.step();

                match self.frame_seq_state {
                    Step0Length => self.handle_length(),
                    Step2LengthAndSweep => {
                        self.handle_length();
                        self.handle_sweep();
                    }
                    Step4Length => self.handle_length(),
                    Step6LengthAndSweep => {
                        self.handle_length();
                        self.handle_sweep();
                    }
                    Step7VolumeEnvelope => self.handle_volume(),
                    Step1Nothing | Step3Nothing | Step5Nothing => {}
                };
            }
        }

        self.ch1.clock();
        self.ch2.clock();
        self.ch3.clock();
        self.ch4.clock();

        self.div_prev = Some(bit_5);

        if self.sender.is_some() && self.sample_counter >= SM83_CLOCK_SPEED {
            self.sample_counter %= SM83_CLOCK_SPEED;
            // Sample the APU

            let ch1_amplitude = self.ch1.amplitude();
            let ch1_left = self.ctrl.output.ch1_left() as u8 as f32 * ch1_amplitude;
            let ch1_right = self.ctrl.output.ch1_right() as u8 as f32 * ch1_amplitude;

            let ch2_amplitude = self.ch2.amplitude();
            let ch2_left = self.ctrl.output.ch2_left() as u8 as f32 * ch2_amplitude;
            let ch2_right = self.ctrl.output.ch2_right() as u8 as f32 * ch2_amplitude;

            let ch3_amplitude = self.ch3.amplitude();
            let ch3_left = self.ctrl.output.ch3_left() as u8 as f32 * ch3_amplitude;
            let ch3_right = self.ctrl.output.ch3_right() as u8 as f32 * ch3_amplitude;

            let ch4_amplitude = self.ch4.amplitude();
            let ch4_left = self.ctrl.output.ch4_left() as u8 as f32 * ch4_amplitude;
            let ch4_right = self.ctrl.output.ch4_right() as u8 as f32 * ch4_amplitude;

            let left = (ch1_left + ch2_left + ch3_left + ch4_left) / 4.0;
            let right = (ch1_right + ch2_right + ch3_right + ch4_right) / 4.0;

            self.buffer.push_back((left, right));
        }
    }

    pub fn set_audio_sender(&mut self, sender: AudioSender<f32>) {
        self.sender = Some(sender);
    }

    pub(crate) fn is_full(&self) -> bool {
        self.buffer.len() >= AUDIO_BUFFER_LEN * CHANNEL_COUNT
    }

    pub(crate) fn flush_samples(&mut self) {
        if let Some(sender) = self.sender.as_ref() {
            while self.buffer.len() >= CHANNEL_COUNT {
                match self.buffer.pop_front() {
                    Some((left, right)) => {
                        sender
                            .send_samples(left, right)
                            .expect("Successfully sent samples across threads");
                    }
                    None => unreachable!(
                        "While loop ensures that there are at least two items in AudioBuffer"
                    ),
                }
            }
        }
    }

    /// 0xFF26 | NR52 - Sound On/Off
    pub(crate) fn set_status(&mut self, byte: u8) {
        self.ctrl.enabled = (byte >> 7) & 0x01 == 0x01;

        if !self.ctrl.enabled {
            self.reset();
        }

        self.ch1.enabled = self.ctrl.enabled;
        self.ch2.enabled = self.ctrl.enabled;
        self.ch3.enabled = self.ctrl.enabled;
        self.ch4.enabled = self.ctrl.enabled;
    }

    fn reset(&mut self) {
        // TODO: Clear readable sound registers

        self.ch1.sweep = Default::default();
        self.ch1.duty = Default::default();
        self.ch1.envelope = Default::default();
        self.ch1.freq_hi = Default::default(); // FIXME: What about frequency low?

        self.ch2.duty = Default::default();
        self.ch2.envelope = Default::default();
        self.ch2.freq_hi = Default::default();

        self.ch3.enabled = Default::default();
        self.ch3.len = Default::default();
        self.ch3.volume = Default::default();
        self.ch3.freq_hi = Default::default();

        self.ch4.len = Default::default();
        self.ch4.envelope = Default::default();
        self.ch4.poly = Default::default();
        self.ch4.freq = Default::default();

        self.ch2 = Default::default();
        self.ch3 = Default::default();
        self.ch4 = Default::default();
        self.ctrl.channel = Default::default();
        self.ctrl.output = Default::default();
    }

    fn clock_length(freq_hi: &FrequencyHigh, length_timer: &mut u16, enabled: &mut bool) {
        if freq_hi.idk() && *length_timer > 0 {
            *length_timer -= 1;

            // Check in this scope ensures (only) the above subtraction
            // made length_timer 0
            if *length_timer == 0 {
                *enabled = false;
            }
        }
    }

    fn clock_length_ch4(freq_data: &Ch4Frequency, length_timer: &mut u16, enabled: &mut bool) {
        if freq_data.idk() && *length_timer > 0 {
            *length_timer -= 1;

            // Check in this scope ensures (only) the above subtraction
            // made length_timer 0
            if *length_timer == 0 {
                *enabled = false;
            }
        }
    }

    fn handle_length(&mut self) {
        Self::clock_length(
            &self.ch1.freq_hi,
            &mut self.ch1.length_timer,
            &mut self.ch1.enabled,
        );

        Self::clock_length(
            &self.ch2.freq_hi,
            &mut self.ch2.length_timer,
            &mut self.ch2.enabled,
        );

        Self::clock_length(
            &self.ch3.freq_hi,
            &mut self.ch3.length_timer,
            &mut self.ch3.enabled,
        );

        Self::clock_length_ch4(
            &self.ch4.freq,
            &mut self.ch4.length_timer,
            &mut self.ch4.enabled,
        );
    }

    fn handle_sweep(&mut self) {
        if self.ch1.sweep_timer > 0 {
            self.ch1.sweep_timer -= 1;
        }

        if self.ch1.sweep_timer == 0 {
            self.ch1.sweep_timer = if self.ch1.sweep.period() != 0 {
                self.ch1.sweep.period()
            } else {
                8
            };

            if self.ch1.sweep_enabled && self.ch1.sweep.period() != 0 {
                let new_freq = self.ch1.calc_sweep_freq();

                if new_freq <= 2047 && self.ch1.sweep.shift_count() != 0 {
                    self.ch1.set_frequency(new_freq);
                    self.ch1.shadow_freq = new_freq & 0x07FF;

                    let _ = self.ch1.calc_sweep_freq();
                }
            }
        }
    }

    fn clock_envelope(envelope: &VolumeEnvelope, period_timer: &mut u8, current_volume: &mut u8) {
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

    fn handle_volume(&mut self) {
        // Channels 1, 2 and 4 have Volume Envelopes

        Self::clock_envelope(
            &self.ch1.envelope,
            &mut self.ch1.period_timer,
            &mut self.ch1.current_volume,
        );

        Self::clock_envelope(
            &self.ch2.envelope,
            &mut self.ch2.period_timer,
            &mut self.ch2.current_volume,
        );

        Self::clock_envelope(
            &self.ch4.envelope,
            &mut self.ch4.period_timer,
            &mut self.ch4.current_volume,
        );
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct SoundControl {
    /// 0xFF24 | NR50 - Channel Control
    channel: ChannelControl,
    /// 0xFF25 | NR51 - Selection of Sound output terminal
    output: SoundOutput,

    enabled: bool,
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
        u8::from(self.output)
    }

    /// 0xFF25 | NR51 - Selection of Sound output terminal
    pub(crate) fn set_output(&mut self, byte: u8) {
        if self.enabled {
            self.output = byte.into();
        }
    }

    /// 0xFF26 | NR52 - Sound On/Off
    pub(crate) fn status(&self, apu: &Apu) -> u8 {
        (self.enabled as u8) << 7
            | (apu.ch4.enabled as u8) << 3
            | (apu.ch3.enabled as u8) << 2
            | (apu.ch2.enabled as u8) << 1
            | apu.ch1.enabled as u8
    }
}

#[derive(Debug, Clone, Copy, Default)]
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
    length_timer: u16,

    freq_timer: u16,
    duty_pos: u8,

    enabled: bool,
}

impl Channel1 {
    fn amplitude(&self) -> f32 {
        let dac_input = self.duty.wave_pattern().amplitude(self.duty_pos) * self.current_volume;

        (dac_input as f32 / 7.5) - 1.0
    }

    fn clock(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            // TODO: Why is this 2048?
            self.freq_timer = (2048 - self.frequency()) * 4;
            self.duty_pos = (self.duty_pos + 1) % 8;
        }
    }

    /// 0xFF10 | NR10 - Channel 1 Sweep Register
    pub(crate) fn sweep(&self) -> u8 {
        u8::from(self.sweep) | 0x80
    }

    /// 0xFF10 | NR10 - Channel 1 Sweep Register
    pub(crate) fn set_sweep(&mut self, byte: u8) {
        if self.enabled {
            self.sweep = byte.into()
        }
    }

    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    pub(crate) fn duty(&self) -> u8 {
        u8::from(self.duty) | 0x3F
    }

    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    pub(crate) fn set_duty(&mut self, byte: u8) {
        if self.enabled {
            self.duty = byte.into();
            self.length_timer = 64 - self.duty.sound_length() as u16;
        }
    }

    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    pub fn envelope(&self) -> u8 {
        u8::from(self.envelope)
    }

    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    pub(crate) fn set_envelope(&mut self, byte: u8) {
        if self.enabled {
            self.envelope = byte.into()
        }
    }

    /// 0xFF13 | NR13 - Channel 1 Frequency low (lower 8 bits only)
    pub(crate) fn set_freq_lo(&mut self, byte: u8) {
        if self.enabled {
            self.freq_lo = byte;
        }
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        u8::from(self.freq_hi) | 0xBF
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
        if self.enabled {
            self.freq_hi = byte.into();

            // If this bit is set, a trigger event occurs
            if self.freq_hi.initial() {
                // Envelope Behaviour during trigger event
                self.period_timer = self.envelope.period();
                self.current_volume = self.envelope.init_vol();

                // Sweep behaviour during trigger event
                self.shadow_freq = self.frequency() & 0x07FF; // Mask should be redundant
                self.sweep_timer = if self.sweep.period() != 0 {
                    self.sweep.period()
                } else {
                    8
                };

                if self.sweep.period() != 0 || self.sweep.shift_count() != 0 {
                    self.sweep_enabled = true;
                }

                if self.sweep.shift_count() != 0 {
                    let _ = self.calc_sweep_freq();
                }

                // Length behaviour during trigger event
                if self.length_timer == 0 {
                    self.length_timer = 64;
                }
            }
        }
    }

    fn calc_sweep_freq(&mut self) -> u16 {
        use SweepDirection::*;
        let shifted_shadow_freq = self.shadow_freq >> self.sweep.shift_count();

        let new_freq = match self.sweep.direction() {
            Increase => self.shadow_freq + shifted_shadow_freq,
            Decrease => self.shadow_freq - shifted_shadow_freq,
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
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel2 {
    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    duty: SoundDuty,
    /// 0xFF17 | NR22 - Channel 2 Volume ENvelope
    envelope: VolumeEnvelope,
    /// 0xFF18 | NR23 - Channel 2 Frequency low (lower 8 bits only)
    freq_lo: u8,
    /// 0xFF19 | NR24 - Channel 2 Frequency high
    freq_hi: FrequencyHigh,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,

    // Length Functionality
    length_timer: u16,

    freq_timer: u16,
    duty_pos: u8,

    enabled: bool,
}

impl Channel2 {
    fn amplitude(&self) -> f32 {
        let dac_input = self.duty.wave_pattern().amplitude(self.duty_pos) * self.current_volume;

        (dac_input as f32 / 7.5) - 1.0
    }

    fn clock(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            // TODO: Why is this 2048?
            self.freq_timer = (2048 - self.frequency()) * 4;
            self.duty_pos = (self.duty_pos + 1) % 8;
        }
    }

    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) fn duty(&self) -> u8 {
        u8::from(self.duty) | 0x3F
    }

    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) fn set_duty(&mut self, byte: u8) {
        if self.enabled {
            self.duty = byte.into();
            self.length_timer = 64 - self.duty.sound_length() as u16;
        }
    }

    /// 0xFF17 | NR22 - Channel 2 Volume ENvelope
    pub(crate) fn envelope(&self) -> u8 {
        u8::from(self.envelope)
    }

    /// 0xFF17 | NR22 - Channel 2 Volume ENvelope
    pub(crate) fn set_envelope(&mut self, byte: u8) {
        if self.enabled {
            self.envelope = byte.into()
        }
    }

    /// 0xFF18 | NR23 - Channel 2 Frequency low (lower 8 bits only)
    pub(crate) fn set_freq_lo(&mut self, byte: u8) {
        if self.enabled {
            self.freq_lo = byte;
        }
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        u8::from(self.freq_hi) | 0xBF
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
        if self.enabled {
            self.freq_hi = byte.into();

            if self.freq_hi.initial() {
                // Envelope behaviour during trigger event
                self.period_timer = self.envelope.period();
                self.current_volume = self.envelope.init_vol();

                // Length behaviour during trigger event
                if self.length_timer == 0 {
                    self.length_timer = 64;
                }
            }
        }
    }

    fn frequency(&self) -> u16 {
        (self.freq_hi.freq_bits() as u16) << 8 | self.freq_lo as u16
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel3 {
    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    enabled: bool,
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
    length_timer: u16,

    freq_timer: u16,
    offset: u8,
}

impl BusIo for Channel3 {
    fn read_byte(&self, addr: u16) -> u8 {
        self.wave_ram[(addr - Self::WAVE_RAM_START_ADDR) as usize]
    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.wave_ram[(addr - Self::WAVE_RAM_START_ADDR) as usize] = byte;
    }
}

impl Channel3 {
    const WAVE_RAM_START_ADDR: u16 = 0xFF30;

    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    pub(crate) fn enabled(&self) -> u8 {
        ((self.enabled as u8) << 7) | 0x7F
    }

    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    pub(crate) fn set_enabled(&mut self, byte: u8) {
        self.enabled = (byte >> 7) & 0x01 == 0x01;
    }

    /// 0xFF1B | NR31 - Sound Length
    pub(crate) fn len(&self) -> u8 {
        self.len | 0xFF
    }

    /// 0xFF1B | NR31 - Sound Length
    pub(crate) fn set_len(&mut self, byte: u8) {
        if self.enabled {
            self.len = byte;
            self.length_timer = 256 - self.len as u16;
        }
    }

    /// 0xFF1C | NR32 - Channel 3 Volume
    pub(crate) fn volume(&self) -> u8 {
        ((self.volume as u8) << 5) | 0x9F
    }

    /// 0xFF1C | NR32 - Channel 3 Volume
    pub(crate) fn set_volume(&mut self, byte: u8) {
        use Ch3Volume::*;

        if self.enabled {
            self.volume = match (byte >> 5) & 0x03 {
                0b00 => Mute,
                0b01 => Full,
                0b10 => Half,
                0b11 => Quarter,
                _ => unreachable!("{:#04X} is not a valid value for Channel3Volume", byte),
            };
        }
    }

    /// 0xFF1D | NR33 - Channel 3 Frequency low (lower 8 bits)
    pub(crate) fn set_freq_lo(&mut self, byte: u8) {
        if self.enabled {
            self.freq_lo = byte;
        }
    }

    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        u8::from(self.freq_hi) | 0xBF
    }

    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
        if self.enabled {
            self.freq_hi = byte.into();

            if self.freq_hi.initial() {
                // Length behaviour during trigger event
                if self.length_timer == 0 {
                    self.length_timer = 256;
                }
            }
        }
    }

    fn amplitude(&self) -> f32 {
        let dac_input = self.read_sample(self.offset) >> self.volume.shift_count();

        (dac_input as f32 / 7.5) - 1.0
    }

    fn clock(&mut self) {
        if self.freq_timer != 0 {
            self.freq_timer -= 1;
        }

        if self.freq_timer == 0 {
            self.freq_timer = (2048 - self.frequency()) * 4;
            self.offset = (self.offset + 1) % 8;
        }
    }

    fn read_sample(&self, index: u8) -> u8 {
        let i = index as usize / 2;

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

#[derive(Debug, Clone, Copy, Default)]
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
    length_timer: u16,

    /// Linear Feedback Shift Register (15-bit)
    lf_shift: u16,

    freq_timer: u16,

    enabled: bool,
}

impl Channel4 {
    /// 0xFF20 | NR41 - Channel 4 Sound Length
    pub(crate) fn len(&self) -> u8 {
        self.len | 0xFF
    }

    /// 0xFF20 | NR41 - Channel 4 Sound Length
    pub(crate) fn set_len(&mut self, byte: u8) {
        if self.enabled {
            self.len = byte & 0x3F;
            self.length_timer = 256 - self.len as u16;
        }
    }

    /// 0xFF21 | NR42 - Channel 4 Volume Envelope
    pub(crate) fn envelope(&self) -> u8 {
        u8::from(self.envelope)
    }

    /// 0xFF21 | NR42 - Channel 4 Volume Envelope
    pub(crate) fn set_envelope(&mut self, byte: u8) {
        if self.enabled {
            self.envelope = byte.into()
        }
    }

    /// 0xFF22 | NR43 - Chanel 4 Polynomial Counter
    pub(crate) fn poly(&self) -> u8 {
        u8::from(self.poly)
    }

    /// 0xFF22 | NR43 - Chanel 4 Polynomial Counter
    pub(crate) fn set_poly(&mut self, byte: u8) {
        if self.enabled {
            self.poly = byte.into();
        }
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn frequency(&self) -> u8 {
        u8::from(self.freq) | 0xBF
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn set_freq_data(&mut self, byte: u8) {
        if self.enabled {
            self.freq = byte.into();

            if self.freq.initial() {
                // Envelope behaviour during trigger event
                self.period_timer = self.envelope.period();
                self.current_volume = self.envelope.init_vol();

                // Length behaviour during trigger event
                if self.length_timer == 0 {
                    self.length_timer = 64;
                }

                // LFSR behaviour during trigger event
                self.lf_shift = 0x7FFF;
            }
        }
    }

    fn amplitude(&self) -> f32 {
        let dac_input = (!self.lf_shift & 0x01) as u8 * self.current_volume;

        (dac_input as f32 / 7.5) - 1.0
    }

    fn clock(&mut self) {
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

    fn divisor(code: u8) -> u8 {
        if code == 0 {
            return 8;
        }

        code << 4
    }
}
