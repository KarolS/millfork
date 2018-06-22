![](logo_transparent.png)

# Millfork

A middle-level programming language targeting 6502-based microcomputers. 

**UNDER DEVELOPMENT, NOT FOR PRODUCTION USE**

For binary releases, see: https://github.com/KarolS/millfork/releases (latest: 0.2.2)

## Features

* multiple targets:

    * Commodore 64 (the primary target)
    
    * Commodore 64 with SuperCPU (experimental, incomplete and very buggy)
    
    * Commodore 16 and Plus/4
    
    * Commodore 128
    
    * Commodore 64/128 running LUnix/LNG 0.21 (experimental)
    
    * Commodore PET
    
    * Commodore Vic-20 (stock or with RAM extensions)
    
    * Famicom/NES (the second most important target)
    
    * Atari 2600 (experimental)
    
    * Atari 8-bit computers
    
    * BBC Micro
    
    * Apple II+/IIe/Enhanced IIe

* inline assembly

* simple macros

* pay only for what you use: not a single byte of memory is used unless for code or explicitly declared variables

* simple memory model that avoids using the stack

* multi-pass optimizer (that will even optimize your hand-written assembly if you ask it to)

## Licensing

The compiler is distributed under GPLv3 (see [LICENSE](LICENSE)).

The standard include files (located in the `include` directory) are distributed under a more permissive Zlib license (see [include/LICENSE](include/LICENSE)).
Therefore, no attribution is needed if you are developing and distributing Millfork programs.

## More info

* [Documentation](docs/index.md)

* [Example programs](examples/README.md)

## Planned features

* more targets: Oric computers, PC-Engine/Turbografx-16, Atari Lynx
 
* support for 65816, SuperFamicom/SNES and Apple IIgs 
