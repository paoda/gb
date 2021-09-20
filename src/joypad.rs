use gilrs::{Button, Event as GamepadEvent, EventType as GamepadEventType};
use winit_input_helper::WinitInputHelper;

#[derive(Debug)]
pub struct Joypad {
    /// 0xFF00 | P1/JOYP - Player 1 Joypad
    pub(crate) p1: u8,
    ext: JoypadState,
    interrupt: bool,
}

impl Default for Joypad {
    fn default() -> Self {
        Self {
            p1: 0xFF,
            ext: Default::default(),
            interrupt: Default::default(),
        }
    }
}

impl Joypad {
    pub(crate) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(crate) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }

    pub(crate) fn update(&mut self, byte: u8) {
        let direction_row = (byte >> 4) & 0x01 == 0x00;
        let action_row = (byte >> 5) & 0x01 == 0x00;

        let updated = match (direction_row, action_row) {
            (true, false) => (byte & 0x30) | self.ext.as_direction_bits(),
            (false, true) => (byte & 0x30) | self.ext.as_action_bits(),
            _ => {
                // TODO: What if both or no rows are selected?

                let merge = self.ext.as_action_bits() | self.ext.as_action_bits();

                (byte & 0x30) | merge
            }
        };

        self.p1 = updated
    }
}

#[derive(Debug, Default)]
struct JoypadState {
    // Direction Row
    dpad_down: ButtonEvent,
    dpad_up: ButtonEvent,
    dpad_left: ButtonEvent,
    dpad_right: ButtonEvent,
    // Action Row
    start: ButtonEvent,
    select: ButtonEvent,
    south: ButtonEvent,
    east: ButtonEvent,
}

impl JoypadState {
    fn as_direction_bits(&self) -> u8 {
        (self.dpad_down as u8) << 3
            | (self.dpad_up as u8) << 2
            | (self.dpad_left as u8) << 1
            | self.dpad_right as u8
    }

    fn as_action_bits(&self) -> u8 {
        (self.start as u8) << 3
            | (self.select as u8) << 2
            | (self.south as u8) << 1
            | self.east as u8
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ButtonEvent {
    Pressed = 0,
    Standby = 1,
}

// used in the context of is_pressed: bool
impl From<bool> for ButtonEvent {
    fn from(value: bool) -> Self {
        match value {
            true => Self::Pressed,
            false => Self::Standby,
        }
    }
}

impl Default for ButtonEvent {
    fn default() -> Self {
        Self::Standby
    }
}

impl ButtonEvent {
    fn update(&mut self, is_pressed: bool, irq: &mut bool) {
        *self = is_pressed.into();

        if let ButtonEvent::Pressed = *self {
            *irq = true;
        }
    }
}

#[inline]
pub fn handle_keyboard_input(pad: &mut Joypad, input: &WinitInputHelper) {
    use winit::event::VirtualKeyCode;

    // TODO: What do I have to do to get a match statement here?

    let state = &mut pad.ext;
    let irq = &mut pad.interrupt;

    if input.key_pressed(VirtualKeyCode::Down) {
        state.dpad_down.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::Down) {
        state.dpad_down.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::Up) {
        state.dpad_up.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::Up) {
        state.dpad_up.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::Left) {
        state.dpad_left.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::Left) {
        state.dpad_left.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::Right) {
        state.dpad_right.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::Right) {
        state.dpad_right.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::Return) {
        state.start.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::Return) {
        state.start.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::RShift) {
        state.select.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::RShift) {
        state.select.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::Z) {
        state.south.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::Z) {
        state.south.update(false, irq);
    }

    if input.key_pressed(VirtualKeyCode::X) {
        state.east.update(true, irq);
    }
    if input.key_released(VirtualKeyCode::X) {
        state.east.update(false, irq);
    }
}

#[inline]
pub fn handle_gamepad_input(pad: &mut Joypad, event: GamepadEvent) {
    use Button::*;
    use GamepadEventType::*;

    let state = &mut pad.ext;
    let irq = &mut pad.interrupt;

    match event.event {
        ButtonPressed(btn, _) => match btn {
            DPadDown => state.dpad_down.update(true, irq),
            DPadUp => state.dpad_up.update(true, irq),
            DPadLeft => state.dpad_left.update(true, irq),
            DPadRight => state.dpad_right.update(true, irq),
            Start => state.start.update(true, irq),
            Select => state.select.update(true, irq),
            South => state.south.update(true, irq),
            East => state.east.update(true, irq),
            _ => {}
        },
        ButtonReleased(btn, _) => match btn {
            DPadDown => state.dpad_down.update(false, irq),
            DPadUp => state.dpad_up.update(false, irq),
            DPadLeft => state.dpad_left.update(false, irq),
            DPadRight => state.dpad_right.update(false, irq),
            Start => state.start.update(false, irq),
            Select => state.select.update(false, irq),
            South => state.south.update(false, irq),
            East => state.east.update(false, irq),
            _ => {}
        },
        _ => {}
    }
}
