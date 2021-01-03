#[derive(Debug, Clone, Copy, Default)]
pub struct Sound {
    pub status: SoundStatus,
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
