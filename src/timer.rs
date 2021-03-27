use crate::Cycles;
use bitfield::bitfield;

// const DIVIDER_REGISTER_HZ: u32 = 16384;

#[derive(Debug, Clone, Copy)]
pub struct Timer {
    pub control: TimerControl,
    pub counter: u8,
    pub modulo: u8,
    pub divider: u16,
    prev_and_result: Option<u8>,
    interrupt: bool,
}

impl Timer {
    pub fn step(&mut self, cycles: Cycles) {
        use TimerSpeed::*;

        for _ in 0..cycles.into() {
            self.divider = self.divider.wrapping_add(1);

            // Get Bit Position
            let pos = match self.control.speed() {
                Hz4096 => 9,
                Hz262144 => 3,
                Hz65536 => 5,
                Hz16384 => 7,
            };

            let bit = (self.divider >> pos) as u8 & 0x01;
            let timer_enable = self.control.enabled() as u8;
            let and_result = bit & timer_enable;

            if let Some(previous) = self.prev_and_result {
                if previous == 0x01 && and_result == 0x00 {
                    // Falling Edge, increase TIMA Regiser
                    self.increment_tima();
                }
            }
            self.prev_and_result = Some(and_result);
        }
    }

    fn increment_tima(&mut self) {
        let (result, did_overflow) = self.counter.overflowing_add(1);

        self.counter = if did_overflow {
            self.interrupt = true;
            self.modulo
        } else {
            result
        }
    }

    pub fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self {
            control: Default::default(),
            counter: 0,
            modulo: 0,
            divider: 0,
            interrupt: false,
            prev_and_result: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TimerSpeed {
    Hz4096 = 0,
    Hz262144 = 1,
    Hz65536 = 2,
    Hz16384 = 3,
}

impl From<u8> for TimerSpeed {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Hz4096,
            0b01 => Self::Hz262144,
            0b10 => Self::Hz65536,
            0b11 => Self::Hz16384,
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
