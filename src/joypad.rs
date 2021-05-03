use bitfield::bitfield;

#[derive(Debug, Clone, Copy, Default)]
pub struct Joypad {
    pub status: JoypadStatus,
    interrupt: bool,
}

impl Joypad {
    pub fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }
}

bitfield! {
    pub struct JoypadStatus(u8);
    impl Debug;
    from into ButtonRowStatus, action_row, set_action_row: 5, 5;
    from into ButtonRowStatus, direction_row, set_direction_row: 4, 4;
    from into ButtonStatus, _down, _set_down: 3, 3;
    from into ButtonStatus, _up, _set_up: 2, 2;
    from into ButtonStatus, _left, _set_left: 1, 1;
    from into ButtonStatus, _right, _set_right: 0, 0;
}

impl Default for JoypadStatus {
    fn default() -> Self {
        Self(0xFF)
    }
}

impl Copy for JoypadStatus {}
impl Clone for JoypadStatus {
    fn clone(&self) -> Self {
        *self
    }
}

impl From<u8> for JoypadStatus {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<JoypadStatus> for u8 {
    fn from(status: JoypadStatus) -> Self {
        status.0
    }
}

impl JoypadStatus {
    pub fn set_down(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_direction_row();
        // }
        self._set_down(is_pressed.into());
    }

    pub fn set_start(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_action_row();
        // }
        self._set_down(is_pressed.into());
    }

    pub fn set_up(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_direction_row();
        // }
        self._set_up(is_pressed.into());
    }

    pub fn set_select(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_action_row();
        // }
        self._set_up(is_pressed.into());
    }

    pub fn set_left(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_direction_row();
        // }
        self._set_left(is_pressed.into());
    }

    pub fn set_b(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_action_row();
        // }
        self._set_left(is_pressed.into());
    }

    pub fn set_right(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_direction_row();
        // }
        self._set_right(is_pressed.into());
    }

    pub fn set_a(&mut self, is_pressed: bool) {
        // if is_pressed {
        //     self.select_action_row();
        // }
        self._set_right(is_pressed.into());
    }

    pub fn select_direction_row(&mut self) {
        use ButtonRowStatus::*;

        self.set_direction_row(Selected);
        self.set_action_row(Deselected);
    }

    pub fn select_action_row(&mut self) {
        use ButtonRowStatus::*;

        self.set_action_row(Selected);
        self.set_direction_row(Deselected);
    }
}

#[derive(Debug, Clone, Copy)]
enum ButtonStatus {
    Pressed = 0,
    Released = 1,
}

impl From<u8> for ButtonStatus {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Pressed,
            0b01 => Self::Released,
            _ => unreachable!("{:#04X} is not a valid value for ButtonStatus", byte),
        }
    }
}

impl From<ButtonStatus> for u8 {
    fn from(status: ButtonStatus) -> Self {
        status as u8
    }
}

impl From<bool> for ButtonStatus {
    fn from(value: bool) -> Self {
        match value {
            true => Self::Pressed,
            false => Self::Released,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ButtonRowStatus {
    Selected,
    Deselected,
}

impl From<u8> for ButtonRowStatus {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Selected,
            0b01 => Self::Deselected,
            _ => unreachable!("{:#04X} is not a valid value for ButtonRowStatus", byte),
        }
    }
}

impl From<ButtonRowStatus> for u8 {
    fn from(status: ButtonRowStatus) -> Self {
        status as u8
    }
}
