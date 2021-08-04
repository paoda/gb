use bitfield::bitfield;

pub(crate) mod ch1 {
    use super::bitfield;

    bitfield! {
        pub struct Sweep(u8);
        impl Debug;
        _period, _: 6, 4;
        from into SweepDirection, _direction, _: 3, 3;
        _shift_count, _: 2, 0;
    }

    impl Sweep {
        pub(crate) fn period(&self) -> u8 {
            self._period()
        }

        pub(crate) fn shift_count(&self) -> u8 {
            self._shift_count()
        }

        pub(crate) fn direction(&self) -> SweepDirection {
            self._direction()
        }
    }

    impl Copy for Sweep {}
    impl Clone for Sweep {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Default for Sweep {
        fn default() -> Self {
            Self(0)
        }
    }

    impl From<u8> for Sweep {
        fn from(byte: u8) -> Self {
            Self(byte)
        }
    }

    impl From<Sweep> for u8 {
        fn from(sweep: Sweep) -> Self {
            sweep.0
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) enum SweepDirection {
        Increase = 0,
        Decrease = 1,
    }

    impl From<u8> for SweepDirection {
        fn from(byte: u8) -> Self {
            match byte & 0x01 {
                0b00 => Self::Increase,
                0b01 => Self::Decrease,
                _ => unreachable!("{:04X} is not a valid value for SweepChange", byte),
            }
        }
    }

    impl From<SweepDirection> for u8 {
        fn from(change: SweepDirection) -> Self {
            change as u8
        }
    }
}

pub(crate) mod ch3 {
    #[derive(Debug, Clone, Copy)]
    pub(crate) enum Volume {
        Mute = 0,
        Full = 1,
        Half = 2,
        Quarter = 3,
    }

    impl Volume {
        pub(crate) fn shift_count(&self) -> u8 {
            use Volume::*;

            match *self {
                Mute => 4,
                Full => 0,
                Half => 1,
                Quarter => 2,
            }
        }
    }

    impl Default for Volume {
        fn default() -> Self {
            Self::Mute
        }
    }
}

pub(crate) mod ch4 {
    use super::bitfield;

    bitfield! {
        pub struct PolynomialCounter(u8);
        impl Debug;
        _shift_count, _: 7, 4;
        from into CounterWidth, _counter_width, _: 3, 3;
        _divisor_code, _: 2, 0;
    }

    impl PolynomialCounter {
        pub(crate) fn divisor_code(&self) -> u8 {
            self._divisor_code()
        }

        pub(crate) fn counter_width(&self) -> CounterWidth {
            self._counter_width()
        }

        pub(crate) fn shift_count(&self) -> u8 {
            self._shift_count()
        }
    }

    impl Copy for PolynomialCounter {}
    impl Clone for PolynomialCounter {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Default for PolynomialCounter {
        fn default() -> Self {
            Self(0)
        }
    }

    impl From<u8> for PolynomialCounter {
        fn from(byte: u8) -> Self {
            Self(byte)
        }
    }

    impl From<PolynomialCounter> for u8 {
        fn from(poly: PolynomialCounter) -> Self {
            poly.0
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) enum CounterWidth {
        Long,  // 15 bits long
        Short, // 7 bits long
    }

    impl From<u8> for CounterWidth {
        fn from(byte: u8) -> Self {
            match byte & 0x01 {
                0b00 => Self::Short,
                0b01 => Self::Long,
                _ => unreachable!("{:#04X} is not a valid value for CounterWidth"),
            }
        }
    }

    impl From<CounterWidth> for u8 {
        fn from(counter_width: CounterWidth) -> Self {
            counter_width as u8
        }
    }

    bitfield! {
        pub struct Frequency(u8);
        impl Debug;
        _initial, _: 7;
        _length_disable, _: 6; // TODO: same as FrequencyHigh, figure out what this is
    }

    impl Frequency {
        pub(crate) fn length_disable(&self) -> bool {
            self._length_disable()
        }

        pub(crate) fn initial(&self) -> bool {
            self._initial()
        }
    }

    impl Copy for Frequency {}
    impl Clone for Frequency {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Default for Frequency {
        fn default() -> Self {
            Self(0)
        }
    }

    impl From<u8> for Frequency {
        fn from(byte: u8) -> Self {
            Self(byte & 0xC0) // Only bits 7 and 6 hold anything of value
        }
    }

    impl From<Frequency> for u8 {
        fn from(state: Frequency) -> Self {
            state.0 & 0x40 // Only bit 6 holds anything of value
        }
    }
}

pub(crate) mod common {
    use super::bitfield;

    bitfield! {
        pub struct FrequencyHigh(u8);
        impl Debug;
        _initial, _: 7;
        _length_disable, _: 6; // TODO: Figure out what the hell this is
        pub freq_bits, set_freq_bits: 2, 0;
    }

    impl FrequencyHigh {
        pub(crate) fn initial(&self) -> bool {
            self._initial()
        }

        pub(crate) fn length_disable(&self) -> bool {
            self._length_disable()
        }
    }

    impl Copy for FrequencyHigh {}
    impl Clone for FrequencyHigh {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Default for FrequencyHigh {
        fn default() -> Self {
            Self(0)
        }
    }

    impl From<u8> for FrequencyHigh {
        fn from(byte: u8) -> Self {
            Self(byte)
        }
    }

    impl From<FrequencyHigh> for u8 {
        fn from(freq: FrequencyHigh) -> Self {
            freq.0 & 0x40 // Only bit 6 can be read
        }
    }

    bitfield! {
        pub struct VolumeEnvelope(u8);
        impl Debug;
        _init_vol, _: 7, 4;
        from into EnvelopeDirection, _direction, _: 3, 3;
        _period, _: 2, 0;
    }

    impl VolumeEnvelope {
        pub(crate) fn init_vol(&self) -> u8 {
            self._init_vol()
        }

        pub(crate) fn direction(&self) -> EnvelopeDirection {
            self._direction()
        }

        pub(crate) fn period(&self) -> u8 {
            self._period()
        }
    }

    impl Copy for VolumeEnvelope {}
    impl Clone for VolumeEnvelope {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Default for VolumeEnvelope {
        fn default() -> Self {
            Self(0)
        }
    }

    impl From<u8> for VolumeEnvelope {
        fn from(byte: u8) -> Self {
            Self(byte)
        }
    }

    impl From<VolumeEnvelope> for u8 {
        fn from(envelope: VolumeEnvelope) -> Self {
            envelope.0
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) enum EnvelopeDirection {
        Decrease = 0,
        Increase = 1,
    }

    impl From<u8> for EnvelopeDirection {
        fn from(byte: u8) -> Self {
            match byte & 0b01 {
                0b00 => Self::Decrease,
                0b01 => Self::Increase,
                _ => unreachable!("{:#04X} is not a valid value for EnvelopeDirection", byte),
            }
        }
    }

    impl From<EnvelopeDirection> for u8 {
        fn from(direction: EnvelopeDirection) -> Self {
            direction as u8
        }
    }

    impl Default for EnvelopeDirection {
        fn default() -> Self {
            Self::Decrease
        }
    }

    bitfield! {
        pub struct SoundDuty(u8);
        impl Debug;
        from into WavePattern, _wave_pattern, _: 7, 6;
        _sound_length, _: 5, 0; // TODO: Getter only used if bit 6 in NR14 is set
    }

    impl SoundDuty {
        pub(crate) fn wave_pattern(&self) -> WavePattern {
            self._wave_pattern()
        }

        pub(crate) fn sound_length(&self) -> u8 {
            self._sound_length()
        }
    }

    impl Copy for SoundDuty {}
    impl Clone for SoundDuty {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl Default for SoundDuty {
        fn default() -> Self {
            Self(0)
        }
    }

    impl From<u8> for SoundDuty {
        fn from(byte: u8) -> Self {
            Self(byte)
        }
    }

    impl From<SoundDuty> for u8 {
        fn from(duty: SoundDuty) -> Self {
            duty.0 & 0xC0 // Only bits 7 and 6 can be read
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) enum WavePattern {
        OneEighth = 0,     // 12.5% ( _-------_-------_------- )
        OneQuarter = 1,    // 25%   ( __------__------__------ )
        OneHalf = 2,       // 50%   ( ____----____----____---- ) (normal)
        ThreeQuarters = 3, // 75%   ( ______--______--______-- )
    }

    impl WavePattern {
        pub(crate) const fn amplitude(&self, index: u8) -> u8 {
            use WavePattern::*;
            let i = 7 - index; // an index of 0 should get the highest bit

            match *self {
                OneEighth => (0b00000001 >> i) & 0x01,
                OneQuarter => (0b10000001 >> i) & 0x01,
                OneHalf => (0b10000111 >> i) & 0x01,
                ThreeQuarters => (0b01111110 >> i) & 0x01,
            }
        }
    }

    impl Default for WavePattern {
        fn default() -> Self {
            Self::OneEighth // Rationale: OneEighth is 0x00
        }
    }

    impl From<WavePattern> for u8 {
        fn from(pattern: WavePattern) -> Self {
            pattern as Self
        }
    }

    impl From<u8> for WavePattern {
        fn from(byte: u8) -> Self {
            match byte & 0b11 {
                0b00 => Self::OneEighth,
                0b01 => Self::OneQuarter,
                0b10 => Self::OneHalf,
                0b11 => Self::ThreeQuarters,
                _ => unreachable!("{:#04X} is not a valid value for WavePattern", byte),
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum FrameSequencerState {
    Step0Length,
    Step1Nothing,
    Step2LengthAndSweep,
    Step3Nothing,
    Step4Length,
    Step5Nothing,
    Step6LengthAndSweep,
    Step7VolumeEnvelope,
}

impl FrameSequencerState {
    pub(crate) fn step(&mut self) {
        use FrameSequencerState::*;

        *self = match *self {
            Step0Length => Step1Nothing,
            Step1Nothing => Step2LengthAndSweep,
            Step2LengthAndSweep => Step3Nothing,
            Step3Nothing => Step4Length,
            Step4Length => Step5Nothing,
            Step5Nothing => Step6LengthAndSweep,
            Step6LengthAndSweep => Step7VolumeEnvelope,
            Step7VolumeEnvelope => Step0Length,
        };
    }
}

impl Default for FrameSequencerState {
    fn default() -> Self {
        Self::Step0Length
    }
}

bitfield! {
    pub struct SoundOutput(u8);
    impl Debug;
    pub ch4_left, _: 7;
    pub ch3_left, _: 6;
    pub ch2_left, _: 5;
    pub ch1_left, _: 4;
    pub ch4_right, _: 3;
    pub ch3_right, _: 2;
    pub ch2_right, _: 1;
    pub ch1_right, _: 0;
}

impl Copy for SoundOutput {}
impl Clone for SoundOutput {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for SoundOutput {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for SoundOutput {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<SoundOutput> for u8 {
    fn from(output: SoundOutput) -> Self {
        output.0
    }
}

bitfield! {
    pub struct ChannelControl(u8);
    impl Debug;
    vin_left, _: 7;
    _left_volume, _: 6, 4;
    vin_right, _: 3;
    _right_volume, _: 2, 0;
}

impl ChannelControl {
    pub(crate) fn left_volume(&self) -> f32 {
        self._left_volume() as f32
    }

    pub(crate) fn right_volume(&self) -> f32 {
        self._right_volume() as f32
    }
}

impl Copy for ChannelControl {}
impl Clone for ChannelControl {
    fn clone(&self) -> Self {
        *self
    }
}

impl Default for ChannelControl {
    fn default() -> Self {
        Self(0)
    }
}

impl From<u8> for ChannelControl {
    fn from(byte: u8) -> Self {
        Self(byte)
    }
}

impl From<ChannelControl> for u8 {
    fn from(ctrl: ChannelControl) -> Self {
        ctrl.0
    }
}
