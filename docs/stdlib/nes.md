[< back to index](../index.md)

# NES/Famicom-oriented modules

## `nes_hardware` module

The `nes_hardware` module is imported automatically on NES targets.

TODO

## `nes_mmc4` module

The `nes_mmc4` module is imported automatically on the NES MMC4 target.
and contains routines related to MMC4 bankswitching.

#### `void set_prg_bank(byte a)`

Changes the $8000-$BFFF PRG bank.

#### `void set_chr_bank0(byte a)`

Changes the CHR bank 0 ($0000-$0fff in the PPU memory space).

The high nibble (0 or 1) selects between `chrrom0` and `chrrom1` segments.
The low nibble L (0-$F) selects a 4K-aligned address in the segment ($L000).

#### `void set_chr_bank1(byte a)`

Changes the CHR bank 1 ($1000-$1fff in the PPU memory space).

The high nibble (0 or 1) selects between `chrrom0` and `chrrom1` segments.
The low nibble L (0-$F) selects a 4K-aligned address in the segment ($L000).

#### `void set_vertical_mirroring()`

Switches nametable mirroring to vertical.

#### `void set_horizontal_mirroring()`

Switches nametable mirroring to horizontal.