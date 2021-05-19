use bitfield::bitfield;

#[derive(Debug, Clone, Copy, Default)]
pub struct Serial {
    pub next: u8,
    pub control: SerialControl,
}

bitfield! {
    pub struct SerialControl(u8);
    impl Debug;
    pub transfer_start, set_transfer_start: 7;
    // pub from into ClockSpeed, speed, set_speed: 1; (CGB Only)
    pub from into ShiftClock, shift, set_shift: 0;
}

impl Copy for SerialControl {}
impl Clone for SerialControl {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for SerialControl {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for SerialControl {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<SerialControl> for u8 {
    fn from(ctrl: SerialControl) -> Self {
        ctrl.0
    }
}

#[derive(Debug, Clone, Copy)]
enum ShiftClock {
    External = 0,
    Internal = 1,
}

impl Default for ShiftClock {
    fn default() -> Self {
        Self::External
    }
}

impl From<u8> for ShiftClock {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::External,
            0b01 => Self::Internal,
            _ => unreachable!("{:#04X} is not a valid value for ShiftClock", byte),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ClockSpeed {
    Normal = 0,
    Fast = 1,
}

impl Default for ClockSpeed {
    fn default() -> Self {
        Self::Normal
    }
}

impl From<u8> for ClockSpeed {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Normal,
            0b01 => Self::Fast,
            _ => unreachable!("{:#04X} is not a valid value for ClockSpeed", byte),
        }
    }
}
