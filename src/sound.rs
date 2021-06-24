use bitfield::bitfield;

const WAVE_PATTERN_RAM_LEN: usize = 0x10;

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Sound {
    pub(crate) ctrl: SoundControl,
    /// Tone & Sweep
    pub(crate) ch1: Channel1,
    /// Tone
    pub(crate) ch2: Channel2,
    /// Wave
    pub(crate) ch3: Channel3,
    /// Noise
    pub(crate) ch4: Channel4,

    // Frame Sequencer
    frame_seq_state: FrameSequencerState,
    div_prev: Option<u8>,
}

impl Sound {
    pub(crate) fn clock(&mut self, div: u16) {
        use FrameSequencerState::*;

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

        self.div_prev = Some(bit_5);
    }

    fn handle_length(&mut self) {
        if self.ch1.freq_hi.idk() {
            if self.ch1.length_timer > 0 {
                self.ch1.length_timer -= 1;

                // Check in this scope ensures (only) the above subtraction
                // made length_timer 0
                if self.ch1.length_timer == 0 {
                    todo!("Disable Channel 1 until next trigger event");
                }
            }
        }

        if self.ch2.freq_hi.idk() {
            if self.ch2.length_timer > 0 {
                self.ch2.length_timer -= 1;

                // Check in this scope ensures (only) the above subtraction
                // made length_timer 0
                if self.ch2.length_timer == 0 {
                    todo!("Disable Channel 2 until next trigger event");
                }
            }
        }

        if self.ch3.freq_hi.idk() {
            if self.ch3.length_timer > 0 {
                self.ch3.length_timer -= 1;

                // Check in this scope ensures (only) the above subtraction
                // made length_timer 0
                if self.ch3.length_timer == 0 {
                    todo!("Disable Channel 3 until next trigger event");
                }
            }
        }

        if self.ch4.freq_data.idk() {
            if self.ch4.length_timer > 0 {
                self.ch4.length_timer -= 1;

                // Check in this scope ensures (only) the above subtraction
                // made length_timer 0
                if self.ch4.length_timer == 0 {
                    todo!("Disable Channel 4 until next trigger event");
                }
            }
        }
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

    fn handle_volume(&mut self) {
        use EnvelopeDirection::*;
        // Channels 1, 2 and 4 have Volume Envelopes

        if self.ch1.envelope.period() != 0 {
            if self.ch1.period_timer > 0 {
                self.ch1.period_timer -= 1;
            }

            if self.ch1.period_timer == 0 {
                self.ch1.period_timer = self.ch1.envelope.period();

                match self.ch1.envelope.direction() {
                    Decrease if self.ch1.current_volume > 0x00 => self.ch1.current_volume -= 1,
                    Increase if self.ch1.current_volume < 0x0F => self.ch1.current_volume += 1,
                    _ => {}
                }
            }
        }

        if self.ch2.envelope.period() != 0 {
            if self.ch2.period_timer > 0 {
                self.ch2.period_timer -= 1;
            }

            if self.ch2.period_timer == 0 {
                self.ch2.period_timer = self.ch2.envelope.period();

                match self.ch2.envelope.direction() {
                    Decrease if self.ch2.current_volume > 0x00 => self.ch2.current_volume -= 1,
                    Increase if self.ch2.current_volume < 0x0F => self.ch2.current_volume += 1,
                    _ => {}
                }
            }
        }

        if self.ch4.envelope.period() != 0 {
            if self.ch4.period_timer > 0 {
                self.ch4.period_timer -= 1;
            }

            if self.ch4.period_timer == 0 {
                self.ch4.period_timer = self.ch4.envelope.period();

                match self.ch4.envelope.direction() {
                    Decrease if self.ch4.current_volume > 0x00 => self.ch4.current_volume -= 1,
                    Increase if self.ch4.current_volume < 0x0F => self.ch4.current_volume += 1,
                    _ => {}
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum FrameSequencerState {
    Step0Length,
    Step1Nothing,
    Step2LengthAndSweep,
    Step3Nothing,
    Step4Length,
    Step5Nothing,
    Step6LengthAndSweep,
    Step7VolumeEnvelope,
}

impl FrameSequencerState {
    fn step(&mut self) {
        use FrameSequencerState::*;

        *self = match *self {
            Step0Length => Step1Nothing,
            Step1Nothing => Step2LengthAndSweep,
            Step2LengthAndSweep => Step3Nothing,
            Step3Nothing => Step4Length,
            Step4Length => Step5Nothing,
            Step5Nothing => Step6LengthAndSweep,
            Step6LengthAndSweep => Step7VolumeEnvelope,
            Step7VolumeEnvelope => Step0Length,
        };
    }
}

impl Default for FrameSequencerState {
    fn default() -> Self {
        Self::Step0Length
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct SoundControl {
    /// 0xFF24 | NR50 - Channel Control
    pub(crate) channel: ChannelControl,
    /// 0xFF25 | NR51 - Selection of Sound output terminal
    pub(crate) output: SoundOutput,
    /// 0xFF26 | NR52 - Sound On/Off
    pub(crate) status: SoundStatus,
}

// TODO: What to do about the separation of freq bits
// across multiple registers?
bitfield! {
    pub struct FrequencyHigh(u8);
    impl Debug;
    initial, set_initial: 7;
    idk, set_idk: 6; // TODO: Figure out what the hell this is
    freq_bits, set_freq_bits: 2, 0;
}

impl Copy for FrequencyHigh {}
impl Clone for FrequencyHigh {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for FrequencyHigh {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for FrequencyHigh {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<FrequencyHigh> for u8 {
    fn from(freq: FrequencyHigh) -> Self {
        freq.0 & 0x40 // Only bit 6 can be read
    }
}

bitfield! {
    pub struct SoundStatus(u8);
    impl Debug;
    pub all_enabled, set_all_enabled: 7;
    pub sound_4, _: 3;
    pub sound_3, _: 2;
    pub sound_2, _: 1;
    pub sound_1, _: 0;
}

impl Copy for SoundStatus {}
impl Clone for SoundStatus {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for SoundStatus {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for SoundStatus {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<SoundStatus> for u8 {
    fn from(status: SoundStatus) -> Self {
        status.0
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel1 {
    /// 0xFF10 | NR10 - Channel 1 Sweep Register
    pub(crate) sweep: Sweep,
    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    duty: SoundDuty,
    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    pub(crate) envelope: VolumeEnvelope,
    /// 0xFF13 | NR13 - Channel 1 Frequency low (lower 8 bits only)
    pub(crate) freq_lo: u8,
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
}

impl Channel1 {
    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    pub(crate) fn duty(&self) -> u8 {
        self.duty.into()
    }

    /// 0xFF11 | NR11 - Channel 1 Sound length / Wave pattern duty
    pub(crate) fn set_duty(&mut self, byte: u8) {
        self.duty = byte.into();
        self.length_timer = 64 - self.duty.sound_length() as u16;
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        self.freq_hi.into()
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
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

    fn calc_sweep_freq(&mut self) -> u16 {
        use SweepDirection::*;
        let shifted_shadow_freq = self.shadow_freq >> self.sweep.shift_count();

        let new_freq = match self.sweep.direction() {
            Increase => self.shadow_freq + shifted_shadow_freq,
            Decrease => self.shadow_freq - shifted_shadow_freq,
        };

        // Overflow check
        if new_freq > 2047 {
            todo!("Frequency failed the overflow check. Disable the channel");
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

bitfield! {
    pub struct Sweep(u8);
    impl Debug;
    period, set_period: 6, 4;
    from into SweepDirection, direction, set_direction: 3, 3;
    shift_count, set_shift_count: 2, 0;
}

impl Copy for Sweep {}
impl Clone for Sweep {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for Sweep {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for Sweep {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<Sweep> for u8 {
    fn from(sweep: Sweep) -> Self {
        sweep.0
    }
}

#[derive(Debug, Clone, Copy)]
enum SweepDirection {
    Increase = 0,
    Decrease = 1,
}

impl From<u8> for SweepDirection {
    fn from(byte: u8) -> Self {
        match byte & 0x01 {
            0b00 => Self::Increase,
            0b01 => Self::Decrease,
            _ => unreachable!("{:04X} is not a valid value for SweepChange", byte),
        }
    }
}

impl From<SweepDirection> for u8 {
    fn from(change: SweepDirection) -> Self {
        change as u8
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel2 {
    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    duty: SoundDuty,
    /// 0xFF17 | NR22 - Channel 2 Volume ENvelope
    pub(crate) envelope: VolumeEnvelope,
    /// 0xFF18 | NR23 - Channel 2 Frequency low (lower 8 bits only)
    pub(crate) freq_lo: u8,
    /// 0xFF19 | NR24 - Channel 2 Frequency high
    freq_hi: FrequencyHigh,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,

    // Length Functionality
    length_timer: u16,
}

impl Channel2 {
    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) fn duty(&self) -> u8 {
        self.duty.into()
    }

    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) fn set_duty(&mut self, byte: u8) {
        self.duty = byte.into();
        self.length_timer = 64 - self.duty.sound_length() as u16;
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        self.freq_hi.into()
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
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

bitfield! {
    pub struct VolumeEnvelope(u8);
    impl Debug;
    pub init_vol, set_init_vol: 7, 4;
    pub from into EnvelopeDirection, direction, set_direction: 3, 3;
    pub period, set_period: 2, 0;
}

impl Copy for VolumeEnvelope {}
impl Clone for VolumeEnvelope {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for VolumeEnvelope {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for VolumeEnvelope {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<VolumeEnvelope> for u8 {
    fn from(envelope: VolumeEnvelope) -> Self {
        envelope.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EnvelopeDirection {
    Decrease = 0,
    Increase = 1,
}

impl From<u8> for EnvelopeDirection {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Decrease,
            0b01 => Self::Increase,
            _ => unreachable!("{:#04X} is not a valid value for EnvelopeDirection", byte),
        }
    }
}

impl From<EnvelopeDirection> for u8 {
    fn from(direction: EnvelopeDirection) -> Self {
        direction as u8
    }
}

impl Default for EnvelopeDirection {
    fn default() -> Self {
        Self::Decrease
    }
}

bitfield! {
   pub struct SoundDuty(u8);
    impl Debug;
   pub from into WavePattern, wave_pattern, set_wave_pattern: 7, 6;
   pub sound_length, _: 5, 0; // TODO: Getter only used if bit 6 in NR14 is set
}

impl Copy for SoundDuty {}
impl Clone for SoundDuty {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for SoundDuty {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for SoundDuty {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<SoundDuty> for u8 {
    fn from(duty: SoundDuty) -> Self {
        duty.0 & 0xC0 // Only bits 7 and 6 can be read
    }
}

#[derive(Debug, Clone, Copy)]
pub enum WavePattern {
    OneEighth = 0,     // 12.5% ( _-------_-------_------- )
    OneQuarter = 1,    // 25%   ( __------__------__------ )
    OneHalf = 2,       // 50%   ( ____----____----____---- ) (normal)
    ThreeQuarters = 3, // 75%   ( ______--______--______-- )
}

impl Default for WavePattern {
    fn default() -> Self {
        Self::OneEighth // Rationale: OneEighth is 0x00
    }
}

impl From<WavePattern> for u8 {
    fn from(pattern: WavePattern) -> Self {
        pattern as Self
    }
}

impl From<u8> for WavePattern {
    fn from(byte: u8) -> Self {
        match byte & 0b11 {
            0b00 => Self::OneEighth,
            0b01 => Self::OneQuarter,
            0b10 => Self::OneHalf,
            0b11 => Self::ThreeQuarters,
            _ => unreachable!("{:#04X} is not a valid value for WavePattern", byte),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel3 {
    /// 0xFF1A | NR30 - Channel 3 Sound on/off
    enabled: bool,
    /// 0xFF1B | NR31 - Sound Length
    len: u8,
    /// 0xFF1C | NR32 - Channel 3 Volume
    volume: Channel3Volume,
    /// 0xFF1D | NR33 - Channel 3 Frequency low (lower 8 bits)
    pub(crate) freq_lo: u8,
    /// 0xFF1E | NR34 - Channel 3 Frequency high
    freq_hi: FrequencyHigh,
    pub(crate) ram: [u8; WAVE_PATTERN_RAM_LEN],

    // Length Functionality
    length_timer: u16,
}

impl Channel3 {
    /// 0xFF1B | NR31 - Sound Length
    pub(crate) fn len(&self) -> u8 {
        self.len
    }

    /// 0xFF1B | NR31 - Sound Length
    pub(crate) fn set_len(&mut self, byte: u8) {
        self.len = byte;
        self.length_timer = 256 - self.len as u16;
    }

    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        self.freq_hi.into()
    }

    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
        self.freq_hi = byte.into();

        if self.freq_hi.initial() {
            // Length behaviour during trigger event
            if self.length_timer == 0 {
                self.length_timer = 256;
            }
        }
    }

    pub(crate) fn enabled(&self) -> u8 {
        self.enabled as u8
    }

    pub(crate) fn set_enabled(&mut self, byte: u8) {
        self.enabled = (byte >> 7) & 0x01 == 0x01;
    }

    pub(crate) fn volume(&self) -> u8 {
        (self.volume as u8) << 5
    }

    pub(crate) fn set_volume(&mut self, byte: u8) {
        use Channel3Volume::*;

        self.volume = match (byte >> 5) & 0x03 {
            0b00 => Mute,
            0b01 => Full,
            0b10 => Half,
            0b11 => Quarter,
            _ => unreachable!("{:#04X} is not a valid value for Channel3Volume", byte),
        };
    }
}

#[derive(Debug, Clone, Copy)]
enum Channel3Volume {
    Mute = 0,
    Full = 1,
    Half = 2,
    Quarter = 3,
}

impl Default for Channel3Volume {
    fn default() -> Self {
        Self::Mute
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel4 {
    /// 0xFF20 | NR41 - Channel 4 Sound Length
    len: u8,
    /// 0xFF21 | NR42 - Channel 4 Volume Envelope
    pub(crate) envelope: VolumeEnvelope,
    /// 0xFF22 | NR43 - Chanel 4 Polynomial Counter
    pub(crate) poly: PolynomialCounter,
    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    freq_data: Channel4Frequency,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,

    // Length Functionality
    length_timer: u16,
}

impl Channel4 {
    /// 0xFF20 | NR41 - Channel 4 Sound Length
    pub(crate) fn len(&self) -> u8 {
        self.len & 0x3F
    }

    /// 0xFF20 | NR41 - Channel 4 Sound Length
    pub(crate) fn set_len(&mut self, byte: u8) {
        self.len = byte & 0x3F;
        self.length_timer = 256 - self.len as u16;
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn freq_data(&self) -> u8 {
        self.freq_data.into()
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn set_freq_data(&mut self, byte: u8) {
        self.freq_data = byte.into();

        if self.freq_data.initial() {
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

bitfield! {
    pub struct PolynomialCounter(u8);
    impl Debug;
    freq, set_freq: 7, 4;
    from into CounterWidth, counter_width, set_counter_width: 3, 3;
    div_ratio, set_div_ratio: 2, 0;
}

impl Copy for PolynomialCounter {}
impl Clone for PolynomialCounter {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for PolynomialCounter {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for PolynomialCounter {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<PolynomialCounter> for u8 {
    fn from(poly: PolynomialCounter) -> Self {
        poly.0
    }
}

#[derive(Debug, Clone, Copy)]
enum CounterWidth {
    Long,  // 15 bits long
    Short, // 7 bits long
}

impl From<u8> for CounterWidth {
    fn from(byte: u8) -> Self {
        match byte & 0x01 {
            0b00 => Self::Short,
            0b01 => Self::Long,
            _ => unreachable!("{:#04X} is not a valid value for CounterWidth"),
        }
    }
}

impl From<CounterWidth> for u8 {
    fn from(counter_width: CounterWidth) -> Self {
        counter_width as u8
    }
}

bitfield! {
    pub struct Channel4Frequency(u8);
    impl Debug;
    initial, set_initial: 7;
    idk, set_idk: 6; // TODO: same as FrequencyHigh, figure out what this is
}

impl Copy for Channel4Frequency {}
impl Clone for Channel4Frequency {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for Channel4Frequency {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for Channel4Frequency {
    fn from(byte: u8) -> Self {
        Self(byte & 0xC0) // Only bits 7 and 6 hold anything of value
    }
}

impl From<Channel4Frequency> for u8 {
    fn from(state: Channel4Frequency) -> Self {
        state.0 & 0x40 // Only bit 6 holds anything of value
    }
}

bitfield! {
    pub struct SoundOutput(u8);
    impl Debug;
    pub snd4_so2, set_snd4_so2: 7;
    pub snd3_so2, set_snd3_so2: 6;
    pub snd2_so2, set_snd2_so2: 5;
    pub snd1_so2, set_snd1_so2: 4;
    pub snd4_so1, set_snd4_so1: 3;
    pub snd3_so1, set_snd3_so1: 2;
    pub snd2_so1, set_snd2_so1: 1;
    pub snd1_so1, set_snd1_so1: 0;
}

impl Copy for SoundOutput {}
impl Clone for SoundOutput {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for SoundOutput {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for SoundOutput {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<SoundOutput> for u8 {
    fn from(output: SoundOutput) -> Self {
        output.0
    }
}

bitfield! {
    pub struct ChannelControl(u8);
    impl Debug;
    pub vin_so2, set_vin_so2: 7;
    pub so2_level, set_so2_level: 6, 4;
    pub vin_so1, set_vin_so1: 3;
    pub so1_level, set_so1_level: 2, 0;
}

impl Copy for ChannelControl {}
impl Clone for ChannelControl {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for ChannelControl {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for ChannelControl {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<ChannelControl> for u8 {
    fn from(ctrl: ChannelControl) -> Self {
        ctrl.0
    }
}
