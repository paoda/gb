use bitfield::bitfield;

#[derive(Debug)]
pub(crate) struct Timer {
    /// 0xFF07 | TAC - Timer Control
    pub(crate) ctrl: TimerControl,
    /// 0xFF05 | TIMA - Timer Counter
    counter: u8,
    /// 0xFF06 | TMA - Timer Modulo
    pub(crate) modulo: u8,
    /// 0xFF04 | DIV - Divider Register
    pub(crate) divider: u16,

    and_result: Option<u8>,
    interrupt: bool,
    state: State,
}

impl Timer {
    pub(crate) fn tick(&mut self) {
        use State::*;
        use TimerSpeed::*;

        match self.state {
            TIMAOverflow(_) | AbortedTIMAOverflow(_) => self.state = self.state.next(),
            LoadTMA => {
                self.counter = self.modulo;
                self.interrupt = true;

                self.state.next();
            }
            Normal => {}
        }

        if let TIMAOverflow(step) | AbortedTIMAOverflow(step) = self.state {
            if step < 3 {
                self.state = self.state.next();
            } else {
                if self.state == TIMAOverflow(step) {
                    self.counter = self.modulo;
                    self.interrupt = true;
                }

                self.state = Normal;
            }
        }

        self.divider = self.divider.wrapping_add(1);

        // Get Bit Position
        let bit = match self.ctrl.speed() {
            Hz4096 => 9,
            Hz262144 => 3,
            Hz65536 => 5,
            Hz16384 => 7,
        };

        let bit = (self.divider >> bit) as u8 & 0x01;
        let new_result = bit & self.ctrl.enabled() as u8;

        if let Some(0x01) = self.and_result {
            if new_result == 0x00 {
                // Falling Edge, increase TIMA Register
                self.inc_counter();
            }
        }

        self.and_result = Some(new_result);
    }

    /// 0xFF05 | TIMA - Timer Counter
    pub(crate) fn tima(&self) -> u8 {
        self.counter
    }

    /// 0xFF05 | TIMA - Timer Counter
    pub(crate) fn set_tima(&mut self, byte: u8) {
        use State::*;

        match self.state {
            Normal | AbortedTIMAOverflow(_) => self.counter = byte,
            TIMAOverflow(step) => {
                self.counter = byte;
                self.state = AbortedTIMAOverflow(step);
            }
            LoadTMA => {}
        }
    }

    pub(crate) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(crate) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }

    fn inc_counter(&mut self) {
        let (sum, did_overflow) = self.counter.overflowing_add(1);
        self.counter = if did_overflow { 0 } else { sum };

        if did_overflow {
            self.state = State::TIMAOverflow(0);
        }
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self {
            ctrl: Default::default(),
            counter: 0,
            modulo: 0,
            divider: 0,
            interrupt: false,
            and_result: None,
            state: State::Normal,
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
        match byte & 0b11 {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    TIMAOverflow(u8),
    AbortedTIMAOverflow(u8),
    Normal,
    LoadTMA,
}

impl State {
    fn next(&self) -> Self {
        use State::*;

        match self {
            Normal | LoadTMA => Normal,
            TIMAOverflow(3) => LoadTMA,
            AbortedTIMAOverflow(3) => Normal,
            TIMAOverflow(step) => TIMAOverflow(step + 1),
            AbortedTIMAOverflow(step) => AbortedTIMAOverflow(step + 1),
        }
    }
}
