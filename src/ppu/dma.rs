use crate::instruction::Cycle;
use std::ops::Range;

#[derive(Debug, Default, Clone)]
pub(crate) struct DmaProcess {
    pub(crate) state: DmaState,
    cycle: Cycle,
    pub(crate) ctrl: DmaControl,
}

impl DmaProcess {
    pub(crate) fn clock(&mut self) -> Option<(u16, u16)> {
        self.cycle += 1;

        match self.state {
            DmaState::Pending => {
                self.cycle += 1;

                // Four Cycles pass before we actually start transferring
                // files

                if self.cycle == 4 {
                    self.state = DmaState::Transferring;
                }

                None
            }
            DmaState::Transferring => {
                if (self.cycle - 4) % 4 == 0 {
                    let i = u32::from((self.cycle - 4) / 4) as usize;
                    let dest = &mut self.ctrl.dest;

                    match self.ctrl.src.as_mut() {
                        Some(src_range) => src_range.nth(i).zip(dest.nth(i)),
                        None => {
                            self.reset();
                            None
                        }
                    }
                } else {
                    None
                }
            }
            DmaState::Disabled => None,
        }
    }

    fn reset(&mut self) {
        self.cycle = Cycle::new(0);
        self.state = DmaState::Disabled;
        self.ctrl.src = None;
        self.ctrl.repr = 0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum DmaState {
    Disabled,
    Pending,
    Transferring,
}

impl Default for DmaState {
    fn default() -> Self {
        Self::Disabled
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DmaControl {
    pub(crate) repr: u8,
    src: Option<Range<u16>>,
    dest: Range<u16>,
}

impl Default for DmaControl {
    fn default() -> Self {
        Self {
            repr: 0,
            src: None,
            dest: 0xFE00..0xFE9F,
        }
    }
}

impl DmaControl {
    fn src(&self) -> Option<&Range<u16>> {
        self.src.as_ref()
    }

    fn dest(&self) -> &Range<u16> {
        &self.dest
    }

    pub fn update(&mut self, byte: u8, state: &mut DmaState) {
        let left = (byte as u16) << 8 | 0x0000;
        let right = (byte as u16) << 8 | 0x009F;

        self.repr = byte;
        self.src = Some(left..right);
        *state = DmaState::Pending;
    }
}

#[cfg(test)]
mod tests {
    use super::{DmaControl, DmaProcess, DmaState};

    #[derive(Debug, Default, Clone)]
    struct MockBus {
        dma: DmaProcess,
    }

    #[test]
    fn dma_control_works() {
        let mut dma_ctrl: DmaControl = Default::default();
        let mut state = DmaState::Disabled;

        assert_eq!(dma_ctrl.src(), None);
        assert_eq!(*dma_ctrl.dest(), 0xFE00..0xFE9F);

        dma_ctrl.update(0xAB, &mut state);
        assert_eq!(dma_ctrl.src(), Some(0xAB00..0xAB9F).as_ref());
        assert_eq!(*dma_ctrl.dest(), 0xFE00..0xFE9F);
    }

    #[test]
    fn ctrl_update_vs_borrow_checker() {
        let mut bus: MockBus = Default::default();
        assert_eq!(bus.dma.state, DmaState::Disabled);

        bus.dma.ctrl.update(0xAB, &mut bus.dma.state);

        assert_eq!(bus.dma.ctrl.src(), Some(0xAB00..0xAB9F).as_ref());
        assert_eq!(bus.dma.state, DmaState::Pending);
    }
}
