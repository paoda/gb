use crate::Cycle;

#[derive(Debug)]
pub(crate) struct DirectMemoryAccess {
    pub(crate) state: DmaState,
    cycle: Cycle,
    /// 0xFF46 | DMA - Transfer and Start Address
    pub(crate) start: DmaAddress,
}

impl Default for DirectMemoryAccess {
    fn default() -> Self {
        Self {
            state: DmaState::Disabled,
            cycle: Default::default(),
            start: Default::default(),
        }
    }
}

impl DirectMemoryAccess {
    pub(crate) fn tick(&mut self) -> Option<(u16, u16)> {
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
                    .start
                    .0
                    .as_mut()
                    .expect("Source Address present during DMA Transfer");

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
        self.cycle = 0;
        self.state = DmaState::Disabled;
        self.start.0 = None;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum DmaState {
    Disabled,
    Pending,
    Transferring,
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct DmaAddress(Option<u16>);

impl DmaAddress {
    pub(crate) fn update(&mut self, byte: u8, state: &mut DmaState) {
        let start = (byte as u16) << 8;

        self.0 = Some(start);
        *state = DmaState::Pending;
    }
}

impl From<DmaAddress> for u8 {
    fn from(ctrl: DmaAddress) -> Self {
        match ctrl.0 {
            Some(addr) => (addr >> 8) as u8,
            None => 0xFF, // TODO: What garbage value should be here?
        }
    }
}
