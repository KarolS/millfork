# Millfork

A middle-level programming language targeting 6502-based microcomputers.

**UNDER DEVELOPMENT, NOT FOR PRODUCTION USE**

## Features

* multiple targets:

    * Commodore 64 (the primary target)
    
    * Commodore 16 and Plus/4
    
    * Commodore 128
    
    * Commodore PET
    
    * Commodore Vic-20 (stock or with RAM extensions)
    
    * Atari 8-bit computers

* inline assembly

* pay only for what you use: not a single byte of memory is used unless for code or explicitly declared variables

* simple memory model that avoids using the stack

* multi-pass optimizer (that will even optimize your hand-written assembly if you ask it to)

## Planned features

* multi-part programs

* more targets: Famicon/NES, BBC Micro/Electron, Oric computers, Apple II, PC-Engine/Turbografx-16, Atari Lynx

* a better optimizer