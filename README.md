![](logo_transparent.png)

# Millfork

A middle-level programming language targeting 6502-based microcomputers. 

Distributed under GPLv3 (see [LICENSE](LICENSE))

**UNDER DEVELOPMENT, NOT FOR PRODUCTION USE**

## Features

* multiple targets:

    * Commodore 64 (the primary target)
    
    * Commodore 16 and Plus/4
    
    * Commodore 128
    
    * Commodore PET
    
    * Commodore Vic-20 (stock or with RAM extensions)
    
    * Atari 8-bit computers
    
    * Apple II+/IIe/Enhanced IIe

* inline assembly

* pay only for what you use: not a single byte of memory is used unless for code or explicitly declared variables

* simple memory model that avoids using the stack

* multi-pass optimizer (that will even optimize your hand-written assembly if you ask it to)

## More info

* [Documentation](doc/README.md)

* [Example programs](examples/README.md)

## Planned features

* multi-part programs

* more targets: Famicon/NES, BBC Micro/Electron, Oric computers, PC-Engine/Turbografx-16, Atari Lynx
