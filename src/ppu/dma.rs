use crate::instruction::Cycle;

#[derive(Debug, Default, Clone)]
pub(crate) struct DmaProcess {
    pub(crate) state: DmaState,
    cycle: Cycle,
    pub(crate) ctrl: DmaControl,
}

impl DmaProcess {
    pub(crate) fn clock(&mut self) -> Option<(u16, u16)> {
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
                self.cycle += 1;

                let src_addr = self
                    .ctrl
                    .src_addr
                    .as_mut()
                    .expect("DMA Transfer Attempted without a known source address");

                let addresses = if (self.cycle - 4) % 4 == 0 {
                    *src_addr += 1;
                    Some((*src_addr, 0xFE00 | (*src_addr & 0x00FF)))
                } else {
                    None
                };

                if self.cycle == 644 {
                    self.reset();

                    return None;
                }

                addresses
            }
            DmaState::Disabled => None,
        }
    }

    pub(crate) fn is_active(&self) -> bool {
        self.state == DmaState::Transferring
    }

    fn reset(&mut self) {
        self.cycle = Cycle::new(0);
        self.state = DmaState::Disabled;
        self.ctrl.src_addr = None;
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
    src_addr: Option<u16>,
}

impl Default for DmaControl {
    fn default() -> Self {
        Self {
            repr: 0,
            src_addr: None,
        }
    }
}

impl DmaControl {
    pub(crate) fn update(&mut self, byte: u8, state: &mut DmaState) {
        let start = (byte as u16) << 8;

        self.repr = byte;
        self.src_addr = Some(start);
        *state = DmaState::Pending;
    }
}
