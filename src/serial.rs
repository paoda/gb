#[derive(Debug, Clone, Copy, Default)]
pub struct Serial {
    next: u8,
    control: SerialControl,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct SerialControl {
    transfer_start: bool,
    speed: ClockSpeed, // CGB Only
    shift: ShiftClock,
}

impl From<u8> for SerialControl {
    fn from(byte: u8) -> Self {
        Self {
            transfer_start: byte >> 7 == 0x01,
            speed: ((byte >> 1) & 0x01).into(),
            shift: (byte & 0x01).into(),
        }
    }
}

impl From<SerialControl> for u8 {
    fn from(control: SerialControl) -> Self {
        (control.transfer_start as u8) << 7 | (control.speed as u8) << 1 | control.shift as u8
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
        match byte {
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
        match byte {
            0b00 => Self::Normal,
            0b01 => Self::Fast,
            _ => unreachable!("{:#04X} is not a valid value for ClockSpeed", byte),
        }
    }
}
