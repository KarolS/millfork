![](logo_transparent.png)

# Millfork

A middle-level programming language targeting 6502-based, 8080-based and Z80-based microcomputers. 

For binary releases, see: [https://github.com/KarolS/millfork/releases](https://github.com/KarolS/millfork/releases)
(latest: 0.3.12).  
For build instructions, see [Build instructions](./COMPILING.md).

## Features

* high performance, due to being designed and optimized for 8-bit microprocessors

* multiple targets:

    * Commodore 64 (the primary target)
    
    * Commodore 64 with SuperCPU (experimental, incomplete and very buggy)
    
    * other Commodore computers: C16, Plus/4, C128, PET, VIC-20 (stock or with RAM extensions)
    
    * other 6502-based machines: Famicom/NES, Atari Lynx, Atari 8-bit computers, BBC Micro, Apple II+/IIe/Enhanced IIe, Atari 2600 (experimental), Commander X16 (experimental)
    
    * Z80-based machines: ZX Spectrum 48k, NEC PC-88, Amstrad CPC, MSX
    
    * CP/M
    
    * Game Boy (experimental)
    
    * MS-DOS (very experimental, via 8080-to-8086 translation)

* multiple supported target processors:

    * well supported: MOS 6502, Ricoh 2A03/2A07, WDC 65C02, Intel 8080, Intel 8085, Zilog Z80
    
    * reasonably well supported: Sharp LR35902, CSG 65CE02
    
    * partially supported: Hudson Soft HuC6280, WDC 65816, Intel 8086

* inline assembly

* simple macros

* pay only for what you use: not a single byte of memory is used unless for code or explicitly declared variables

* a simple memory model that avoids using the stack

* multi-pass whole-program optimizer (that will even optimize your hand-written assembly if you ask it to)

* support for multi-file programs (Commodore only) and banked cartridges

## Licensing

The compiler is distributed under GPLv3 (see [LICENSE](LICENSE)).

The standard include files (located in the `include` directory) are distributed under a more permissive Zlib license (see [include/LICENSE](include/LICENSE)).
Therefore, no attribution is needed if you are developing and distributing Millfork programs.

The documentation is distributed under the [CC-0 license](https://creativecommons.org/publicdomain/zero/1.0/).

## More info

* [Documentation](docs/README.md) (external version: [https://millfork.readthedocs.io](https://millfork.readthedocs.io))

* [Example programs](examples/README.md)

