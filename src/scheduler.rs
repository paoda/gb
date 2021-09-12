use crate::Cycle;
use std::collections::BinaryHeap;

#[derive(Debug)]
pub(crate) struct Scheduler {
    timestamp: Cycle,
    queue: BinaryHeap<Event>,
}

impl Scheduler {
    pub(crate) fn init() -> Self {
        let mut scheduler = Self {
            timestamp: Default::default(),
            queue: Default::default(),
        };

        scheduler.push(Event {
            kind: EventKind::TimestampOverflow,
            timestamp: Cycle::MAX,
            cb: |_delay| panic!("Reached Cycle::MAX"),
        });

        scheduler
    }

    pub(crate) fn push(&mut self, event: Event) {
        self.queue.push(event);
    }

    pub(crate) fn step(&mut self, cycles: Cycle) {
        self.timestamp += cycles;

        loop {
            let should_pop = match self.queue.peek() {
                Some(event) => self.timestamp >= event.timestamp,
                None => false,
            };

            if !should_pop {
                break;
            }

            let event = self.queue.pop().expect("Pop Event from Scheduler Queue");

            (event.cb)(self.timestamp - event.timestamp);
        }
    }
}

#[derive(Debug)]
pub(crate) struct Event {
    kind: EventKind,
    cb: fn(Cycle),
    pub(crate) timestamp: Cycle,
}

impl Eq for Event {}
impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.timestamp == other.timestamp
    }
}

impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Event {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.timestamp.cmp(&other.timestamp)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EventKind {
    TimestampOverflow,
}
