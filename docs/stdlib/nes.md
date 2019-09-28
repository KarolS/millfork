[< back to index](../doc_index.md)

# NES/Famicom-oriented modules

## nes_hardware

The `nes_hardware` module is imported automatically on NES targets. 
It contains defintions for the NES's memory-mapped registers, as well 
as some joypad and PPU management routines. 

#### `PPU memory-mapped registers`

Named memory locations for interfacing with the PPU's memory-mapped registers.

Available variables:

*    `byte ppu_ctrl   @$2000`
*    `byte ppu_mask   @$2001`
*    `byte ppu_status @$2002`
*    `byte oam_addr   @$2003`
*    `byte oam_data   @$2004`
*    `byte ppu_scroll @$2005`
*    `byte ppu_addr   @$2006`
*    `byte ppu_data   @$2007`
*    `byte oam_dma    @$4014`

#### `byte strobe_joypad()`

Updates joypad1 by querying for new button states.

#### `byte read_joypad1()`

Get joypad1's state as a byte.

#### `byte read_joypad2()`

Get joypad2's state as a byte.

#### `void simulate_reset()`

Simulates a hardware reset by jumping to the reset vector,
which then calls main().

#### `void ppu_set_addr(word ax)`

Sets the PPU to point at the VRAM address at ax, usually in preparation 
for a write via ppu_write_data().

#### `byte read_ppu_status()`

Gets the PPU status byte.

#### `void ppu_set_scroll(byte a, byte x)`

Sets the PPU scroll register. Parameter a defines the horizontal 
(X-axis) scroll value, and parameter x defines the vertical (Y-axis) 
scroll value.

#### `void ppu_write_data(byte a)`

Writes a byte to the PPU's VRAM at the address the PPU 
is currently pointing to. Usually used after a call to ppu_set_addr(). 

#### `void ppu_oam_dma_write(byte a)`

Initiates a DMA transfer of 256 bytes from CPU memory address $xx00-$xxFF 
to PPU OAM memory, where xx is the hexadecimal representation of parameter a.

## nes_mmc4

The `nes_mmc4` module is imported automatically on the NES MMC4 target 
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

## nes_joy

Provides an interface for reading joypads that is compatible with the `joy` module.

#### `alias input_a = input_btn`

1 if A button pressed, 0 id not pressed.

#### `byte input_b`

1 if B button pressed, 0 id not pressed.

#### `byte input_select`

1 if Select button pressed, 0 id not pressed.

#### `byte input_start`

1 if Start button pressed, 0 id not pressed.

#### `void read_joy1()`

Reads the joypad from the port 1.

#### `void read_joy2()`

Reads the joypad from the port 2.

#### `void read_also_joy1()`

Reads the joypad from the port 1 and adds its readouts to the current readouts.

#### `void read_also_joy2()`

Reads the joypad from the port 2 and adds its readouts to the current readouts.

#### `void nes_reset_joy()`
#### `alias reset_joy = nes_reset_joy!`

Resets the state variables.

## nes_joy1_default

Defines the joystick in port 1 as the default joystick.

#### `alias read_joy = read_joy1`
