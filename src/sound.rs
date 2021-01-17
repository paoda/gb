#[derive(Debug, Clone, Copy, Default)]
pub struct Sound {
    pub status: SoundStatus,
    pub ch1: Channel1,
    pub select: SoundOutputSelect,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SoundStatus {
    pub all_enabled: bool, // You can actually write to this one.
    sound_4: bool,
    sound_3: bool,
    sound_2: bool,
    sound_1: bool,
}

impl From<u8> for SoundStatus {
    fn from(byte: u8) -> Self {
        Self {
            all_enabled: (byte >> 7) & 0x01 == 0x01,
            sound_4: (byte >> 3) & 0x01 == 0x01,
            sound_3: (byte >> 2) & 0x01 == 0x01,
            sound_2: (byte >> 1) & 0x01 == 0x01,
            sound_1: (byte >> 0) & 0x01 == 0x01,
        }
    }
}

impl From<SoundStatus> for u8 {
    fn from(status: SoundStatus) -> Self {
        (status.all_enabled as u8) << 7
            | (status.sound_4 as u8) << 3
            | (status.sound_3 as u8) << 2
            | (status.sound_2 as u8) << 1
            | (status.sound_1 as u8) << 0
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Channel1 {
    pub sound_duty: SoundDuty,
    pub vol_envelope: VolumeEnvelope,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct VolumeEnvelope {
    init_vol: u8,
    direction: EnvelopeDirection,
    sweep_count: u8,
}

impl From<u8> for VolumeEnvelope {
    fn from(byte: u8) -> Self {
        Self {
            init_vol: byte >> 4,                    // Bit 7 -> 4
            direction: ((byte >> 3) & 0x01).into(), // Bit 3
            sweep_count: byte & 0x07,               // Bits 2 -> 0
        }
    }
}

impl From<VolumeEnvelope> for u8 {
    fn from(envelope: VolumeEnvelope) -> Self {
        let dir_bit: u8 = envelope.direction.into();
        let mut byte = envelope.init_vol << 4;
        byte |= dir_bit << 3;
        byte |= envelope.sweep_count;
        byte
    }
}

#[derive(Debug, Clone, Copy)]
enum EnvelopeDirection {
    Decrease,
    Increase,
}

impl From<u8> for EnvelopeDirection {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Decrease,
            0b01 => Self::Increase,
            _ => unreachable!("{:#04X} is not a possible value for EnvelopeDirection"),
        }
    }
}

impl From<EnvelopeDirection> for u8 {
    fn from(envelope: EnvelopeDirection) -> Self {
        match envelope {
            EnvelopeDirection::Decrease => 0b00,
            EnvelopeDirection::Increase => 0b01,
        }
    }
}

impl Default for EnvelopeDirection {
    fn default() -> Self {
        Self::Decrease // Reasoning: Decrease is 0
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SoundDuty {
    wave_pattern: WaveDuty,
    sound_length: u8, //
}

impl From<u8> for SoundDuty {
    fn from(byte: u8) -> Self {
        let pattern = byte >> 6; // Get bytes 7 and 6
        let sound_length = byte & 0x3F; // Clear bytes 7 and 6

        SoundDuty {
            wave_pattern: pattern.into(),
            sound_length,
        }
    }
}

impl From<SoundDuty> for u8 {
    fn from(duty: SoundDuty) -> Self {
        let mut byte: u8 = duty.wave_pattern.into();
        byte = (byte << 6) | duty.sound_length;
        byte
    }
}

#[derive(Debug, Clone, Copy)]
pub enum WaveDuty {
    OneEighth,     // 12.5% ( _-------_-------_------- )
    OneQuarter,    // 25%   ( __------__------__------ )
    OneHalf,       // 50%   ( ____----____----____---- ) (normal)
    ThreeQuarters, // 75%   ( ______--______--______-- )
}

impl Default for WaveDuty {
    fn default() -> Self {
        Self::OneEighth // Rationale: OneEighth is 0x00
    }
}

impl From<WaveDuty> for u8 {
    fn from(wave: WaveDuty) -> Self {
        match wave {
            WaveDuty::OneEighth => 0b00,
            WaveDuty::OneQuarter => 0b01,
            WaveDuty::OneHalf => 0b10,
            WaveDuty::ThreeQuarters => 0b11,
        }
    }
}

impl From<u8> for WaveDuty {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::OneEighth,
            0b01 => Self::OneQuarter,
            0b10 => Self::OneHalf,
            0b11 => Self::ThreeQuarters,
            _ => unreachable!("{:#04X} is not a valid value for a Sound Wave", byte),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SoundOutputSelect {
    pub snd4_term2: bool,
    pub snd3_term2: bool,
    pub snd2_term2: bool,
    pub snd1_term2: bool,
    pub snd4_term1: bool,
    pub snd3_term1: bool,
    pub snd2_term1: bool,
    pub snd1_term1: bool,
}

impl From<SoundOutputSelect> for u8 {
    fn from(select: SoundOutputSelect) -> Self {
        (select.snd4_term2 as u8) << 7
            | (select.snd3_term2 as u8) << 6
            | (select.snd2_term2 as u8) << 5
            | (select.snd1_term2 as u8) << 4
            | (select.snd4_term1 as u8) << 3
            | (select.snd3_term1 as u8) << 2
            | (select.snd2_term1 as u8) << 1
            | (select.snd1_term1 as u8)
    }
}

impl From<u8> for SoundOutputSelect {
    fn from(byte: u8) -> Self {
        Self {
            snd4_term2: byte >> 7 == 0x01,
            snd3_term2: (byte >> 6) & 0x01 == 0x01,
            snd2_term2: (byte >> 5) & 0x01 == 0x01,
            snd1_term2: (byte >> 4) & 0x01 == 0x01,
            snd4_term1: (byte >> 3) & 0x01 == 0x01,
            snd3_term1: (byte >> 2) & 0x01 == 0x01,
            snd2_term1: (byte >> 1) & 0x01 == 0x01,
            snd1_term1: byte & 0x01 == 0x01,
        }
    }
}
