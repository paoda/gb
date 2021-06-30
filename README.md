# Rekai's Gameboy Emulator
[![Build Status](https://ci.paoda.moe/api/badges/paoda/gb/status.svg)](https://ci.paoda.moe/paoda/gb)

### Status
* Passes Blargg's cpu_instrs Test
* Renders Background & Window Tiles 
* Implements a PPU FIFO


### Notes
* [gameboy-logs](https://github.com/wheremyfoodat/Gameboy-logs) suggests that there are still some underlying problems with the cpu implementation
* The Sprite FIFO does not work as expected yet
* Sound is neither emulated nor stubbed. Upon writing / reading to a APU related register the emulator will panic. 
* Code cleanup is pending completion of some minimum viable product of the emulator
