#[derive(Debug, Clone, Copy, Default)]
pub struct Interrupt {
    pub flag: InterruptFlag,
    pub enable: InterruptEnable,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct InterruptEnable {
    vblank: bool,
    lcd_stat: bool,
    timer: bool,
    serial: bool,
    joypad: bool,
}
impl From<u8> for InterruptEnable {
    fn from(byte: u8) -> Self {
        Self {
            vblank: (byte >> 0 & 0x01) == 0x01,
            lcd_stat: (byte >> 1 & 0x01) == 0x01,
            timer: (byte >> 2 & 0x01) == 0x01,
            serial: (byte >> 3 & 0x01) == 0x01,
            joypad: (byte >> 4 & 0x01) == 0x01,
        }
    }
}

impl From<InterruptEnable> for u8 {
    fn from(flag: InterruptEnable) -> Self {
        (flag.joypad as u8) << 4
            | (flag.serial as u8) << 3
            | (flag.timer as u8) << 2
            | (flag.lcd_stat as u8) << 1
            | (flag.vblank as u8) << 0
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct InterruptFlag {
    vblank: bool,
    lcd_stat: bool,
    timer: bool,
    serial: bool,
    joypad: bool,
}

impl From<u8> for InterruptFlag {
    fn from(byte: u8) -> Self {
        Self {
            vblank: (byte >> 0 & 0x01) == 0x01,
            lcd_stat: (byte >> 1 & 0x01) == 0x01,
            timer: (byte >> 2 & 0x01) == 0x01,
            serial: (byte >> 3 & 0x01) == 0x01,
            joypad: (byte >> 4 & 0x01) == 0x01,
        }
    }
}

impl From<InterruptFlag> for u8 {
    fn from(flag: InterruptFlag) -> Self {
        (flag.joypad as u8) << 4
            | (flag.serial as u8) << 3
            | (flag.timer as u8) << 2
            | (flag.lcd_stat as u8) << 1
            | (flag.vblank as u8) << 0
    }
}

enum InterruptType {
    VBlank,
    LCDStat,
    Timer,
    Serial,
    Joypad,
}
