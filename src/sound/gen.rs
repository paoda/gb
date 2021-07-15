use super::{AUDIO_BUFFER_LEN, CHANNEL_COUNT, SAMPLE_RATE};
use crossbeam_channel::{Receiver, Sender, TrySendError};
use rodio::Source;
use std::collections::VecDeque;

pub struct AudioMPSC;

impl AudioMPSC {
    pub fn new() -> (AudioSender<f32>, AudioReceiver<f32>) {
        // TODO: Can we provide an upper limit for this?
        // The larger this channel is, the more lag there is between the Audio and
        // Emulator
        let (send, recv) = crossbeam_channel::unbounded();

        (AudioSender { inner: send }, AudioReceiver { inner: recv })
    }
}

#[derive(Debug, Clone)]
pub struct AudioSender<T> {
    inner: Sender<T>,
}

impl<T> AudioSender<T> {
    pub(crate) fn send_samples(&self, left: T, right: T) -> Result<(), TrySendError<T>> {
        self.inner.try_send(left).and(self.inner.try_send(right))?;
        Ok(())
    }
}

pub struct AudioReceiver<T> {
    inner: Receiver<T>,
}

impl<T> Iterator for AudioReceiver<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: Should this never return none?
        self.inner.recv().ok()
    }
}

impl<T: rodio::Sample> Source for AudioReceiver<T> {
    fn current_frame_len(&self) -> Option<usize> {
        // A frame changes when the samples rate or
        // number of channels change. This will never happen, so
        // we return
        None
    }

    fn channels(&self) -> u16 {
        // The Gameboy supports two channels
        CHANNEL_COUNT as u16
    }

    fn sample_rate(&self) -> u32 {
        SAMPLE_RATE
    }

    fn total_duration(&self) -> Option<std::time::Duration> {
        // The duration of this source is infinite
        None
    }
}

#[derive(Debug, Clone)]
pub(crate) struct AudioBuffer<T> {
    inner: VecDeque<T>,
}

impl<T> Default for AudioBuffer<T> {
    fn default() -> Self {
        Self {
            inner: VecDeque::with_capacity(AUDIO_BUFFER_LEN * CHANNEL_COUNT),
        }
    }
}

impl<T> AudioBuffer<T> {
    pub(crate) fn push_back(&mut self, value: T) {
        self.inner.push_back(value)
    }

    pub(crate) fn pop_front(&mut self) -> Option<T> {
        self.inner.pop_front()
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }
}
