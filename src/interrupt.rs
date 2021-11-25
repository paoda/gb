use bitfield::bitfield;

#[derive(Debug)]
pub(crate) struct Interrupt {
    pub(crate) flag: InterruptFlag,
    pub(crate) enable: InterruptEnable,
}

impl Default for Interrupt {
    fn default() -> Self {
        Self {
            flag: InterruptFlag(0),
            enable: InterruptEnable(0),
        }
    }
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
