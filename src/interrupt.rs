use bitfield::bitfield;

#[derive(Debug, Clone, Copy, Default)]
pub struct Interrupt {
    pub flag: InterruptFlag,
    pub enable: InterruptEnable,
}

bitfield! {
    pub struct InterruptEnable(u8);
    impl Debug;
    _vblank, _set_vblank: 0;
    _lcd_stat, _set_lcd_stat: 1;
    _timer, _set_timer: 2;
    _serial, _set_serial: 3;
    _joypad, _set_joypad: 4;
}

// TODO: Is this the correct behaviour? (I think not)
impl InterruptEnable {
    pub fn vblank(&self) -> bool {
        self._vblank()
    }

    pub fn lcd_stat(&self) -> bool {
        self._lcd_stat()
    }

    pub fn timer(&self) -> bool {
        self._timer()
    }

    pub fn serial(&self) -> bool {
        self._serial()
    }

    pub fn joypad(&self) -> bool {
        self._joypad()
    }

    pub fn set_vblank(&self, flag: &mut InterruptFlag, value: bool) {
        let prev = self._vblank();

        if !prev && value {
            flag.set_vblank(true);
        }

        flag.set_vblank(value)
    }

    pub fn set_lcd_stat(&self, flag: &mut InterruptFlag, value: bool) {
        let prev = self._lcd_stat();

        if !prev && value {
            flag.set_lcd_stat(true);
        }

        flag.set_lcd_stat(value)
    }

    pub fn set_timer(&self, flag: &mut InterruptFlag, value: bool) {
        let prev = self._timer();

        if !prev && value {
            flag.set_timer(true);
        }

        flag.set_timer(value)
    }

    pub fn set_serial(&self, flag: &mut InterruptFlag, value: bool) {
        let prev = self._serial();

        if !prev && value {
            flag.set_serial(true);
        }

        flag.set_serial(value)
    }

    pub fn set_joypad(&self, flag: &mut InterruptFlag, value: bool) {
        let prev = self._joypad();

        if !prev && value {
            flag.set_joypad(true);
        }

        flag.set_joypad(value)
    }
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

enum InterruptType {
    VBlank,
    LCDStat,
    Timer,
    Serial,
    Joypad,
}
