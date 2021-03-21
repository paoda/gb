use bitfield::bitfield;

#[derive(Debug, Clone, Copy, Default)]
pub struct Interrupt {
    pub flag: InterruptFlag,
    pub enable: InterruptEnable,
}

bitfield! {
    pub struct InterruptEnable(u8);
    impl Debug;
    pub vblank, set_vblank: 0;
    pub lcd_stat, set_lcd_stat: 1;
    pub timer, set_timer: 2;
    pub serial, set_serial: 3;
    pub joypad, set_joypad: 4;
}

impl Copy for InterruptEnable {}
impl Clone for InterruptEnable {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for InterruptEnable {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for InterruptEnable {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<InterruptEnable> for u8 {
    fn from(enable: InterruptEnable) -> Self {
        enable.0
    }
}

bitfield! {
    pub struct InterruptFlag(u8);
    impl Debug;
    pub vblank, set_vblank: 0;
    pub lcd_stat, set_lcd_stat: 1;
    pub timer, set_timer: 2;
    pub serial, set_serial: 3;
    pub joypad, set_joypad: 4;
}

impl Copy for InterruptFlag {}
impl Clone for InterruptFlag {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for InterruptFlag {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for InterruptFlag {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<InterruptFlag> for u8 {
    fn from(flag: InterruptFlag) -> Self {
        flag.0
    }
}
