# Rekai's Gameboy Emulator
[![Build Status](https://ci.paoda.moe/api/badges/paoda/gb/status.svg)](https://ci.paoda.moe/paoda/gb)

### Status
* From [Blargg Test ROMs](https://github.com/L-P/blargg-test-roms/)
    * [x] cpu_instrs
    * [ ] instr_timing (kind of)
    * [x] mem_timing
    * [x] mem_timing-2
    * [ ] dmg_sound (partial) 
* [x] [dmg-acid2](https://github.com/mattcurrie/dmg-acid2)
* From [mooneye-gb](https://github.com/Gekkio/mooneye-gb):
    * Cartridges:
        * [x] MBC1
        * [ ] MBC1M
        * [x] MBC2
        * [x] MBC5
* Implements a cycle-accurate PPU FIFO
    * Doesn't \*exactly\* work just yet

Supports: ROM-only, MBC1, MBC2, MBC3 and MBC5 games.



### Controls
Controls are defined [here](https://git.musuka.dev/paoda/gb/src/branch/main/src/joypad.rs#L114)

Key | Button
--- | ---
<kbd>X</kbd> | B
<kbd>Z</kbd> | A
<kbd>Enter</kbd> | START
<kbd>Shift</kbd> | SELECT

Then use the Arrow keys for the D-Pad

### Credits
The Boot ROM found in the `bin/` directory was made by [Optix](https://github.com/Hacktix) over [here](https://github.com/Hacktix/Bootix)