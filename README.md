![](logo_transparent.png)

# Millfork

A middle-level programming language targeting 6502-based microcomputers. 

Distributed under GPLv3 (see [LICENSE](LICENSE))

**UNDER DEVELOPMENT, NOT FOR PRODUCTION USE**

For binary releases, see: https://github.com/KarolS/millfork/releases (latest: 0.2)

## Features

* multiple targets:

    * Commodore 64 (the primary target)
    
    * Commodore 64 with SuperCPU (experimental, incomplete and very buggy)
    
    * Commodore 16 and Plus/4
    
    * Commodore 128
    
    * Commodore PET
    
    * Commodore Vic-20 (stock or with RAM extensions)
    
    * Famicom/NES (the second most important target)
    
    * Atari 8-bit computers
    
    * Apple II+/IIe/Enhanced IIe

* inline assembly

* simple macros

* pay only for what you use: not a single byte of memory is used unless for code or explicitly declared variables

* simple memory model that avoids using the stack

* multi-pass optimizer (that will even optimize your hand-written assembly if you ask it to)

## More info

* [Documentation](doc/README.md)

* [Example programs](examples/README.md)

## Planned features

* more targets: BBC Micro/Electron, Oric computers, PC-Engine/Turbografx-16, Atari Lynx
 
* support for 65816, SuperFamicom/SNES and Apple IIgs 
