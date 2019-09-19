[< back to index](../doc_index.md)
                                                      
# Target platforms

Currently, Millfork supports creating disk- or tape-based programs
for Commodore, Apple, BBC and Atari 8-bit computers, NEC PC-88, ZX Spectrum 48k, Amstrad CPC, CP/M,
and cartridge-based programs for Commodore 64, VIC-20, MSX, Famicom/NES and Atari 2600,
but it may be expanded to support other 6502-based and Z80-based platforms in the future.

To add a custom platform yourself, see [the custom platform adding guide](./custom-platform.md).

If you are compiling for a cartridge-based target,
you need to take special precautions; see [the ROM vs RAM guide](./rom-vs-ram.md)

## Supported platforms

The following platforms are currently supported:

* `c64` – Commodore 64.
The compiler emits PRG files, not disk or tape images.
If you want to create a program consisting of multiple PRG files,
see [the Commodore programming guide](./commodore-programming-guide.md) for more info. 

* `c64_crt8k` – Commodore 64, 8K ROM cartridge

* `c64_crt16k` – Commodore 64, 16K ROM cartridge

* `c64_ef_small` – Commodore 64, EasyFlash cartridge, 72K of program code. 
See [the EasyFlash programming guide](./easyflash-programming-guide.md) for more info.

* `c64_scpu` – Commodore 64 with SuperCPU in emulation mode

* `c64_scpu16` – Commodore 64 with SuperCPU in native, 16-bit mode (very buggy)

* `lunix` – Commodore 64 or 128 running LUnix/LNG 0.21
Certain language features work differently on this target.
Read [the LUnix programming guide](./lunix-programming-guide.md) for more info.

* `c64jp` – Japanese version of Commodore 64 (different memory layout and character set)

* `c16` – Commodore 16

* `plus4` – Commodore Plus/4

* `vic20` – Commodore VIC-20 without memory expansion

* `vic20_3k` – Commodore VIC-20 with 3K memory expansion

* `vic20_8k` – Commodore VIC-20 with 8K or 16K memory expansion

* `vic20_a000` – Commodore VIC-20, 8K ROM cartridge at $A000

* `c128` – Commodore 128 in its native mode

* `pet` – Commodore PET

* `nes_small` – a tiny 32K PRGROM + 8K CHRROM Famicom/NES program, using iNES mapper 0 (NROM).
The compiler emits NES files.

* `nes_mcc4` – a 128K PRGROM + 128K CHRROM + extra 8KRAM Famicom/NES program, using iNES mapper 10 (MMC4)  
For more complex programs, you need to create your own "platform" definition.  
Read [the NES programming guide](./famicom-programming-guide.md) for more info.

* `atari_lynx` – Atari Lynx

* `vcs` – Atari VCS (also known as Atari 2600), 4K cartridge (experimental)

* `a8` – Atari 8-bit computers.
The compiler emits XEX files, not disk or tape images.

* `bbcmicro` – BBC Micro model B (32k RAM).
The compiler only emits raw binaries, not disk images.
Read [the BBC Micro programming guide](./bbcmicro-programming-guide.md) for more info.

* `apple2` – Apple II+/IIe/Enhanced IIe.
The compiler only emits raw binaries, not disk images.
Read [the Apple 2 programming guide](./apple2-programming-guide.md) for more info.

* `pc88` – NEC PC-88.
The compiler emits bootable disk images.

* `cpc464` – Amstrad CPC 464.
The compiler only emits raw binaries, not disk images.
Read [the Amstrad CPC programming guide](./cpc-programming-guide.md) for more info.

* `zxspectrum` – Sinclair ZX Spectrum 48k.
The compiler emits tape images.

* `zxspectrum_8080` – Sinclair ZX Spectrum 48k, using only Intel 8080 instructions

* `msx_crt` – MSX 16kB cartridge, using only 16kB RAM

* `gb_small` – a tiny 32K Game Boy program. (experimental)
The compiler emits GB files.
Read [the Game Boy programming guide](./gb-programming-guide.md) for more info.

* `cpm` – CP/M on Intel 8080.
The compiler emits COM files.

* `cpm_z80` – CP/M on Z80

* `dos_com` – a COM file for DOS on IBM PC. (very experimental)

* `x16_experimental` – Commander X16; very experimental,
as both the target configuration *and* the device itself are in their experimental phases.

The primary and most tested platform is Commodore 64.
