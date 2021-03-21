use bitfield::bitfield;

use crate::instruction::Cycles;

#[derive(Debug, Clone, Copy)]
pub struct Timer {
    pub control: TimerControl,
    pub counter: u8,
}

impl Timer {
    pub fn step(&mut self, _cycles: Cycles) {
        //
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self {
            control: Default::default(),
            counter: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TimerSpeed {
    Freq4096Hz = 0,
    Freq262144Hz = 1,
    Freq65536Hz = 2,
    Freq16384Hz = 3,
}

impl From<u8> for TimerSpeed {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Freq4096Hz,
            0b01 => Self::Freq262144Hz,
            0b10 => Self::Freq65536Hz,
            0b11 => Self::Freq16384Hz,
            _ => unreachable!("{:#04X} is not a valid value for TimerSpeed", byte),
        }
    }
}

impl From<TimerSpeed> for u8 {
    fn from(speed: TimerSpeed) -> Self {
        speed as Self
    }
}

bitfield! {
    pub struct TimerControl(u8);
    impl Debug;
    pub enabled, set_enabled: 2;
    pub from into TimerSpeed, speed, set_speed: 1, 0;
}

impl Copy for TimerControl {}
impl Clone for TimerControl {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for TimerControl {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for TimerControl {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<TimerControl> for u8 {
    fn from(ctrl: TimerControl) -> Self {
        ctrl.0
    }
}
