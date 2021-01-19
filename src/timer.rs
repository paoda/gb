use crate::instruction::Cycles;

#[derive(Debug, Clone, Copy)]
pub struct Timer {
    pub control: TimerControl,
}

impl Timer {
    pub fn step(&mut self, _cycles: Cycles) {
        //
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self {
            control: 0x00.into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum TimerSpeed {
    Freq4096Hz,
    Freq262144Hz,
    Freq65536Hz,
    Freq16384Hz,
}

impl From<u8> for TimerSpeed {
    fn from(byte: u8) -> Self {
        match byte {
            0x00 => Self::Freq4096Hz,
            0x01 => Self::Freq262144Hz,
            0x10 => Self::Freq65536Hz,
            0x11 => Self::Freq16384Hz,
            _ => unreachable!(),
        }
    }
}

impl From<TimerSpeed> for u8 {
    fn from(speed: TimerSpeed) -> Self {
        match speed {
            TimerSpeed::Freq4096Hz => 0x00,
            TimerSpeed::Freq262144Hz => 0x01,
            TimerSpeed::Freq65536Hz => 0x10,
            TimerSpeed::Freq16384Hz => 0x11,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TimerControl {
    enabled: bool,
    speed: TimerSpeed,
}

impl From<u8> for TimerControl {
    fn from(byte: u8) -> Self {
        let byte = byte & 0x07; // Clear everything but last 3 bits

        Self {
            enabled: (byte >> 2) == 0x01,
            speed: (byte & 0x03).into(), // Clear everything but last 2 bits
        }
    }
}

impl From<TimerControl> for u8 {
    fn from(control: TimerControl) -> Self {
        let byte: u8 = control.speed.into(); // Get bit 1 and 0.

        (byte & !(1u8 << 2)) | ((control.enabled as u8) << 2) // specifically manibulate bit 2
    }
}
