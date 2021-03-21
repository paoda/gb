use crate::Cycles;
use crate::LR35902_CLOCK_SPEED;
use bitfield::bitfield;

const DIVIDER_REGISTER_HZ: u32 = 16384;

#[derive(Debug, Clone, Copy)]
pub struct Timer {
    pub control: TimerControl,
    pub counter: u8,
    pub modulo: u8,
    pub divider: u8,
    divider_cycles: Cycles,
    timer_cycles: Cycles,
    interrupt: bool,
}

impl Timer {
    pub fn step(&mut self, cycles: Cycles) {
        self.timer_cycles += cycles;
        self.divider_cycles += cycles;

        // Divider Register
        let divider_wait = Cycles::new(LR35902_CLOCK_SPEED / DIVIDER_REGISTER_HZ);
        let timer_wait = self.timer_cycles();

        if self.divider_cycles >= divider_wait {
            // The Divider Timer has ticked
            self.divider_cycles %= divider_wait;

            self.divider = self.divider.wrapping_add(1);
        }

        if self.control.enabled() {
            if self.timer_cycles >= timer_wait {
                // The Timer has ticked
                self.timer_cycles %= timer_wait;

                let (result, did_overflow) = self.counter.overflowing_add(1);
                self.counter = if did_overflow {
                    self.interrupt = true;
                    self.modulo
                } else {
                    result
                };
            }
        }
    }

    fn timer_cycles(&self) -> Cycles {
        let difference = match self.control.speed() {
            TimerSpeed::Hz4096 => LR35902_CLOCK_SPEED / 4096,
            TimerSpeed::Hz262144 => LR35902_CLOCK_SPEED / 262144,
            TimerSpeed::Hz65536 => LR35902_CLOCK_SPEED / 65536,
            TimerSpeed::Hz16384 => LR35902_CLOCK_SPEED / 16384,
        };

        Cycles::new(difference)
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
            timer_cycles: Default::default(),
            divider_cycles: Default::default(),
            counter: 0,
            modulo: 0,
            divider: 0,
            interrupt: false,
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
