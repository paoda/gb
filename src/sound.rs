use crate::instruction::Cycles;
use bitfield::bitfield;
#[derive(Debug, Clone, Copy, Default)]
pub struct Sound {
    pub control: SoundControl,
    pub ch1: Channel1,
}

impl Sound {
    pub fn step(&mut self, _cycles: Cycles) {
        //
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SoundControl {
    pub channel: ChannelControl,
    pub output: SoundOutput,
    pub status: SoundStatus,
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
        freq.0
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

pub fn get_11bit_freq(low: &FrequencyLow, high: FrequencyHigh) -> u16 {
    let high_bits = high.0 & 0b111;

    (low.0 as u16) << 8 | ((high_bits as u16) << 4)
}

#[derive(Debug, Clone, Copy)]
enum FrequencyType {
    Counter = 0,
    Consecutive = 1,
}

impl From<u8> for FrequencyType {
    fn from(byte: u8) -> Self {
        match byte {
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
pub struct Channel1 {
    pub sound_duty: SoundDuty,
    pub vol_envelope: VolumeEnvelope,
    pub freq_hi: FrequencyHigh,
    pub freq_lo: FrequencyLow,
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
        match byte {
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
        duty.0
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
        pattern as u8
    }
}

impl From<u8> for WavePattern {
    fn from(byte: u8) -> Self {
        match byte {
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
