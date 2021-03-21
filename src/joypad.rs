use bitfield::bitfield;

#[derive(Debug, Clone, Copy, Default)]
pub struct Joypad {
    pub status: JoypadStatus,
    pub interrupt: bool,
}

bitfield! {
    pub struct JoypadStatus(u8);
    impl Debug;
    pub from into ButtonSelection, action_buttons, select_action_buttons: 5, 5;
    pub from into ButtonSelection, direction_buttons, select_direction_buttons: 4, 4;
    _down, _set_down: 3;
    _up, _set_up: 2;
    _left, _set_left: 1;
    _right, _set_right: 0;
}

impl Copy for JoypadStatus {}
impl Clone for JoypadStatus {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for JoypadStatus {
    fn default() -> Self {
        // Selected Direction Buttons
        // All Buttons are not pressed
        Self(0b00011111)
    }
}

impl From<u8> for JoypadStatus {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<JoypadStatus> for u8 {
    fn from(joypad: JoypadStatus) -> Self {
        joypad.0
    }
}

impl JoypadStatus {
    pub fn start(&self) -> bool {
        self.down()
    }

    pub fn set_start(&mut self, pressed: bool) -> Option<JoypadAction> {
        self.set_down(pressed)
    }

    pub fn select(&self) -> bool {
        self.up()
    }

    pub fn set_select(&mut self, pressed: bool) -> Option<JoypadAction> {
        self.set_up(pressed)
    }

    pub fn b(&self) -> bool {
        self.left()
    }

    pub fn set_b(&mut self, pressed: bool) -> Option<JoypadAction> {
        self.set_left(pressed)
    }

    pub fn a(&self) -> bool {
        self.right()
    }

    pub fn set_a(&mut self, pressed: bool) -> Option<JoypadAction> {
        self.set_right(pressed)
    }

    pub fn down(&self) -> bool {
        self._down()
    }

    pub fn set_down(&mut self, pressed: bool) -> Option<JoypadAction> {
        self._set_down(pressed);

        if !self._down() && pressed {
            Some(JoypadAction::TriggerInterrupt)
        } else {
            None
        }
    }

    pub fn up(&self) -> bool {
        self._up()
    }

    pub fn set_up(&mut self, pressed: bool) -> Option<JoypadAction> {
        self._set_up(pressed);

        if !self._up() && pressed {
            Some(JoypadAction::TriggerInterrupt)
        } else {
            None
        }
    }

    pub fn left(&self) -> bool {
        self._left()
    }

    pub fn set_left(&mut self, pressed: bool) -> Option<JoypadAction> {
        self._set_left(pressed);

        if !self._left() && pressed {
            Some(JoypadAction::TriggerInterrupt)
        } else {
            None
        }
    }

    pub fn right(&self) -> bool {
        self._right()
    }

    pub fn set_right(&mut self, pressed: bool) -> Option<JoypadAction> {
        self._set_right(pressed);

        if !self._right() && pressed {
            Some(JoypadAction::TriggerInterrupt)
        } else {
            None
        }
    }
}

pub enum JoypadAction {
    TriggerInterrupt,
}

#[derive(Debug, Clone, Copy)]
pub enum ButtonSelection {
    Selected = 0,
    NotSelected,
}

impl From<u8> for ButtonSelection {
    fn from(byte: u8) -> Self {
        match byte {
            0b00 => Self::Selected,
            0b01 => Self::NotSelected,
            _ => unreachable!("{:#04X} is not a valid value for ButtonSelection", byte),
        }
    }
}

impl From<ButtonSelection> for u8 {
    fn from(selection: ButtonSelection) -> Self {
        selection as Self
    }
}
