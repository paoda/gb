use bitfield::bitfield;

#[derive(Debug, Clone, Copy, Default)]
pub struct Joypad {
    pub status: JoypadStatus,
    pub interrupt: bool,
}

impl Joypad {
    pub(crate) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(crate) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }
}

bitfield! {
    pub struct JoypadStatus(u8);
    impl Debug;
    from into RowState, action_row, set_action_row: 5, 5;
    from into RowState, direction_row, set_direction_row: 4, 4;
    from into ButtonState, down_start, _set_down_start: 3, 3;
    from into ButtonState, up_select, _set_up_select: 2, 2;
    from into ButtonState, left_b, _set_left_b: 1, 1;
    from into ButtonState, right_a, _set_right_a: 0, 0;
}

impl JoypadStatus {
    pub(crate) fn update(&mut self, byte: u8) {
        // Bytes 3 -> 0 are Read Only
        let mask = 0b00001111;

        let read_only = self.0 & mask;
        self.0 = (byte & !mask) | read_only;
    }
}

impl JoypadStatus {
    pub fn set_down_start(&mut self, state: ButtonState, int: &mut bool) {
        if !(*int) {
            *int = self.down_start() == ButtonState::Released && state == ButtonState::Pressed;
        }

        self._set_down_start(state);
    }

    pub fn set_up_select(&mut self, state: ButtonState, int: &mut bool) {
        if !(*int) {
            *int = self.up_select() == ButtonState::Released && state == ButtonState::Pressed;
        }

        self._set_up_select(state);
    }

    pub fn set_left_b(&mut self, state: ButtonState, int: &mut bool) {
        if !(*int) {
            *int = self.left_b() == ButtonState::Released && state == ButtonState::Pressed;
        }

        self._set_left_b(state);
    }

    pub fn set_right_a(&mut self, state: ButtonState, int: &mut bool) {
        if !(*int) {
            *int = self.right_a() == ButtonState::Released && state == ButtonState::Pressed;
        }

        self._set_right_a(state);
    }
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

impl From<JoypadStatus> for u8 {
    fn from(status: JoypadStatus) -> Self {
        status.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ButtonState {
    Pressed = 0,
    Released = 1,
}

impl From<u8> for ButtonState {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Pressed,
            0b01 => Self::Released,
            _ => unreachable!("{:#04X} is not a valid value for ButtonStatus", byte),
        }
    }
}

impl From<ButtonState> for u8 {
    fn from(status: ButtonState) -> Self {
        status as u8
    }
}

impl From<bool> for ButtonState {
    fn from(value: bool) -> Self {
        match value {
            true => Self::Pressed,
            false => Self::Released,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum RowState {
    Selected,
    Deselected,
}

impl From<u8> for RowState {
    fn from(byte: u8) -> Self {
        match byte & 0b01 {
            0b00 => Self::Selected,
            0b01 => Self::Deselected,
            _ => unreachable!("{:#04X} is not a valid value for ButtonRowStatus", byte),
        }
    }
}

impl From<RowState> for u8 {
    fn from(status: RowState) -> Self {
        status as u8
    }
}
