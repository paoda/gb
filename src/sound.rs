use bitfield::bitfield;
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Sound {
    pub(crate) ctrl: SoundControl,
    pub(crate) ch1: Channel1,
    pub(crate) ch2: Channel2,
}

impl Sound {
    pub(crate) fn clock(&mut self) {
        //
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
    pub _, set_initial: 7;
    pub from into FrequencyType, get_freq_type, set_freq_type: 6;
    pub _, set_freq_bits: 2, 0;
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
    pub struct FrequencyLow(u8);
    impl Debug;
    pub _, set_freq_bits: 7, 0;
}

impl Copy for FrequencyLow {}
impl Clone for FrequencyLow {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for FrequencyLow {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for FrequencyLow {
    fn from(byte: u8) -> Self {
        Self(byte)
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
    pub(crate) freq_lo: FrequencyLow,
    /// 0xFF14 | NR14 - Channel 1 Frequency high
    pub(crate) freq_hi: FrequencyHigh,
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
    pub(crate) freq_lo: FrequencyLow,
    /// 0xFF19 | NR24 - Channel 2 Frequency high
    pub(crate) freq_hi: FrequencyHigh,
}

bitfield! {
    pub struct VolumeEnvelope(u8);
    impl Debug;
    pub init_vol, set_init_vol: 7, 4;
    pub from into EnvelopeDirection, direction, set_direction: 3;
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
enum EnvelopeDirection {
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
