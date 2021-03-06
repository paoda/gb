use rodio::Source;
use rtrb::{Consumer, Producer, PushError, RingBuffer};

pub(crate) const SAMPLE_RATE: u32 = 48000; // Hz
const CHANNEL_COUNT: usize = 2;
const BUFFER_CAPACITY: usize = 2048 * CHANNEL_COUNT; // # of samples * the # of channels

pub fn init<T>() -> (SampleProducer<T>, SampleConsumer<T>) {
    let (prod, cons) = RingBuffer::new(BUFFER_CAPACITY);
    (
        SampleProducer { inner: prod },
        SampleConsumer { inner: cons },
    )
}

pub struct SampleProducer<T> {
    inner: Producer<T>,
}

impl<T> SampleProducer<T> {
    pub(crate) fn push(&mut self, value: T) -> Result<(), PushError<T>> {
        self.inner.push(value)
    }

    #[allow(dead_code)]
    pub(crate) fn available(&self) -> bool {
        self.inner.slots() > 2
    }

    pub(crate) fn available_blocking(&self) -> bool {
        loop {
            if self.inner.slots() > 2 {
                break true;
            }
        }
    }
}

impl<T> std::fmt::Debug for SampleProducer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("SampleProducer<{}>", std::any::type_name::<T>()))
            .finish_non_exhaustive()
    }
}

pub struct SampleConsumer<T> {
    inner: Consumer<T>,
}

impl Iterator for SampleConsumer<f32> {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        // As of 2021-07-28, PopError can only be Empty
        Some(self.inner.pop().unwrap_or_default())
    }
}

impl Source for SampleConsumer<f32> {
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
