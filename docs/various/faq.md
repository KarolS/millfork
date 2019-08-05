[< back to index](../doc_index.md)

# Frequently Asked Questions

### Who is the target audience?

Millfork was designed to be a language for developers for old 8-bit platforms,
mostly game developers, who have little use for advanced features of C, but don't have time to write assembly.

### What was the inspiration?

The main inspirations was Atalan, but also Quetzalcoatl, Batari BASIC and NESHLA.
Sadly, Atalan has been abandoned and the compiler has been left in a non-working state.
The goal of Millfork is to succeed where Atalan failed. 

### What platforms are supported?

Large programs in Millfork have been developed for Commodore 64.

Millfork was also tested (via emulators) to run trivial programs on other 8-bit Commodore computers, 
Atari 8-bit computers, Apple II, BBC Micro, ZX Spectrum 48k, NEC PC-88, MSX, CP/M, NES, Game Boy, Atari 2600 and MS-DOS.

Support for other devices using supported processors can be easily added, usually without modifying the compiler.

### What microprocessors are supported?

* MOS 6502 and its descendants: 6510, 65C02, Ricoh 2A03, and to a lesser degree CSG 65CE02, Hudson Soft HuC6280 and WDC 65816. 6509 is not supported and will not be.

* Intel 8080, Intel 8085, Zilog Z80, Sharp LR35902 (also known as GBZ80)

* There is also partial experimental support for Intel 8086, via automatic 8080-to-8086 translation.
The generated code is very large and very slow.

* Support for Motorola 6809 is coming in the future.

### Why Millfork when I can use assembly?

* Assembly will not be portable. If you want to target both 6502 and Z80, you'd have to maintain two separate codebases.

* Millfork is more productive. The programmer doesn't have to worry about register allocation or variable sizes.

### Why Millfork when I can use C?

* Millfork is usually a bit faster.

* No runtime, so it's easier to create small programs.

* Many features usually found in advanced assemblers but rarely found in high-level languages are also available in Millfork,
like hygienic macros, binary file inclusion, explicit memory layout, formulaic array initialization.

* Millfork handles text encodings more carefully.

* A wide variety of integer types of almost arbitrary sizes.

* Semantics designed to suit 8-bit microprocessors, so usually less explicit casting is required.

* Low-level things like the carry after arithmetic operations or single bytes of larger variables.

* Built-in decimal arithmetic support.

* Easy interfacing with assembly.

### This sounds like Millfork beats C in every department, right?

Unfortunately not:

* Millfork is very picky. It avoids compiling complex expressions, especially those involving larger variables.

* Integer math support is not very complete.

* Floating point math support is absent.

* Pointer arithmetic is very limited.

* Millfork preprocessor is less powerful than C preprocessor.

* There is no support for linking with external libraries not written in Millfork.
You either need to rewrite foreign assembly into Millfork assembly syntax,
or generate a static binary and link it manually using the `file` directive.

Since the compiler is a work-in-progress, some of the mentioned issues might be improved upon in the future.

### Why is it called Millfork?

It stands for **MI**ddle **L**evel **L**anguage **FOR** **K**ommodore computers.

(There's also a mining town in Utah called Mill Fork, which, as fitting a compiler for obsolete machines, is currently abandoned.) 

### ‟Commodore” isn't spelt with K!

Shh.

