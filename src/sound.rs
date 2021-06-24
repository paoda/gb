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
                    Step0Length => todo!(),
                    Step2LengthAndSweep => todo!(),
                    Step4Length => todo!(),
                    Step6LengthAndSweep => todo!(),
                    Step7VolumeEnvelope => {
                        use EnvelopeDirection::*;
                        // Channels 1, 2 and 4 have Volume Envelopes

                        if self.ch1.envelope.sweep_count() != 0 {
                            if self.ch1.period_timer > 0 {
                                self.ch1.period_timer -= 1;
                            }

                            if self.ch1.period_timer == 0 {
                                self.ch1.period_timer = self.ch1.envelope.sweep_count();

                                match self.ch1.envelope.direction() {
                                    Decrease if self.ch1.current_volume > 0x00 => {
                                        self.ch1.current_volume -= 1
                                    }
                                    Increase if self.ch1.current_volume < 0x0F => {
                                        self.ch1.current_volume += 1
                                    }
                                    _ => {}
                                }
                            }
                        }

                        if self.ch2.envelope.sweep_count() != 0 {
                            if self.ch2.period_timer > 0 {
                                self.ch2.period_timer -= 1;
                            }

                            if self.ch2.period_timer == 0 {
                                self.ch2.period_timer = self.ch2.envelope.sweep_count();

                                match self.ch2.envelope.direction() {
                                    Decrease if self.ch2.current_volume > 0x00 => {
                                        self.ch2.current_volume -= 1
                                    }
                                    Increase if self.ch2.current_volume < 0x0F => {
                                        self.ch2.current_volume += 1
                                    }
                                    _ => {}
                                }
                            }
                        }

                        if self.ch4.envelope.sweep_count() != 0 {
                            if self.ch4.period_timer > 0 {
                                self.ch4.period_timer -= 1;
                            }

                            if self.ch4.period_timer == 0 {
                                self.ch4.period_timer = self.ch4.envelope.sweep_count();

                                match self.ch4.envelope.direction() {
                                    Decrease if self.ch4.current_volume > 0x00 => {
                                        self.ch4.current_volume -= 1
                                    }
                                    Increase if self.ch4.current_volume < 0x0F => {
                                        self.ch4.current_volume += 1
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    Step1Nothing | Step3Nothing | Step5Nothing => {}
                };
            }
        }

        self.div_prev = Some(bit_5);
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
    pub fn step(&mut self) {
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
    initial, _set_initial: 7;
    pub from into FrequencyType, get_freq_type, set_freq_type: 6;
    pub _, set_freq_bits: 2, 0;
}

impl FrequencyHigh {
    pub(crate) fn set_initial(&mut self, value: bool, ch1: &mut Channel1) {
        self._set_initial(value);
    }
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

#[derive(Debug, Clone, Copy)]
enum FrequencyType {
    Counter = 0,
    Consecutive = 1,
}

impl From<u8> for FrequencyType {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Counter,
            0b01 => Self::Consecutive,
            _ => unreachable!("{:#04X} is not a valid value for FrequencyType"),
        }
    }
}

impl From<FrequencyType> for u8 {
    fn from(freq_type: FrequencyType) -> Self {
        freq_type as u8
    }
}

impl Default for FrequencyType {
    fn default() -> Self {
        Self::Counter
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
    pub(crate) duty: SoundDuty,
    /// 0xFF12 | NR12 - Channel 1 Volume Envelope
    pub(crate) envelope: VolumeEnvelope,
    /// 0xFF13 | NR13 - Channel 1 Frequency low (lower 8 bits only)
    pub(crate) freq_lo: u8,
    /// 0xFF14 | NR14 - Channel 1 Frequency high
    freq_hi: FrequencyHigh,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,
}

impl Channel1 {
    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        self.freq_hi.into()
    }

    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
        self.freq_hi = byte.into();

        if self.freq_hi.initial() {
            self.period_timer = self.envelope.sweep_count();
            self.current_volume = self.envelope.init_vol();
        }
    }
}

bitfield! {
    pub struct Sweep(u8);
    impl Debug;
    time, set_time: 6, 4;
    from into SweepChange, change, set_change: 3, 3;
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
pub(crate) enum SweepChange {
    Additive = 0,
    Subtractive = 1,
}

impl From<u8> for SweepChange {
    fn from(byte: u8) -> Self {
        match byte & 0x01 {
            0b00 => Self::Additive,
            0b01 => Self::Subtractive,
            _ => unreachable!("{:04X} is not a valid value for SweepChange", byte),
        }
    }
}

impl From<SweepChange> for u8 {
    fn from(change: SweepChange) -> Self {
        change as u8
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Channel2 {
    /// 0xFF16 | NR21 - Channel 2 Sound length / Wave Pattern Duty
    pub(crate) duty: SoundDuty,
    /// 0xFF17 | NR22 - Channel 2 Volume ENvelope
    pub(crate) envelope: VolumeEnvelope,
    /// 0xFF18 | NR23 - Channel 2 Frequency low (lower 8 bits only)
    pub(crate) freq_lo: u8,
    /// 0xFF19 | NR24 - Channel 2 Frequency high
    freq_hi: FrequencyHigh,

    // Envelope Functionality
    period_timer: u8,
    current_volume: u8,
}

impl Channel2 {
    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn freq_hi(&self) -> u8 {
        self.freq_hi.into()
    }

    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) fn set_freq_hi(&mut self, byte: u8) {
        self.freq_hi = byte.into();

        if self.freq_hi.initial() {
            self.period_timer = self.envelope.sweep_count();
            self.current_volume = self.envelope.init_vol();
        }
    }
}

bitfield! {
    pub struct VolumeEnvelope(u8);
    impl Debug;
    pub init_vol, set_init_vol: 7, 4;
    pub from into EnvelopeDirection, direction, set_direction: 3, 3;
    pub sweep_count, set_sweep_count: 2, 0;
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
   pub _, set_sound_length: 5, 0; // TODO: Getter only used if bit 6 in NR14 is set
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
    pub(crate) len: u8,
    /// 0xFF1C | NR32 - Channel 3 Volume
    volume: Channel3Volume,
    /// 0xFF1D | NR33 - Channel 3 Frequency low (lower 8 bits)
    pub(crate) freq_lo: u8,
    /// 0xFF1E | NR34 - Channel 3 Frequency high
    pub(crate) freq_hi: FrequencyHigh,
    pub(crate) ram: [u8; WAVE_PATTERN_RAM_LEN],
}

impl Channel3 {
    pub fn enabled(&self) -> u8 {
        self.enabled as u8
    }

    pub fn set_enabled(&mut self, byte: u8) {
        self.enabled = (byte >> 7) & 0x01 == 0x01;
    }

    pub fn volume(&self) -> u8 {
        (self.volume as u8) << 5
    }

    pub fn set_volume(&mut self, byte: u8) {
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
}

impl Channel4 {
    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn freq_data(&self) -> u8 {
        self.freq_data.into()
    }

    /// 0xFF23 | NR44 - Channel 4 Counter / Consecutive Selector and Restart
    pub(crate) fn set_freq_data(&mut self, byte: u8) {
        self.freq_data = byte.into();

        if self.freq_data.initial() {
            self.period_timer = self.envelope.sweep_count();
            self.current_volume = self.envelope.init_vol();
        }
    }
}

impl Channel4 {
    pub(crate) fn len(&self) -> u8 {
        self.len & 0x3F
    }

    pub(crate) fn set_len(&mut self, byte: u8) {
        self.len = byte & 0x3F;
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
pub(crate) enum CounterWidth {
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
    from into FrequencyType, freq_type, set_freq_type: 6, 6;
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
