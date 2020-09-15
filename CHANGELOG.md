# Change log

## 0.3.22 (2020-09-15)

* Added local labels in assembly.

* Added alternate decimal operators (with `$` instead of `'`).

* **Potentially breaking change!** Identifiers no longer can end with `$`.

* Added `z80next` as an alternate name for the ZX Spectrum Next's processor (#55).

* Added encodings: `brascii`, `macroman`, `dmcs`, `lics`.

* Improved some error messages.

* Fix: interrupt functions written in assembly no longer have the default prologue (#62).

* Fixed the `a8_os` module (#58).

* Fixed evaluation of division of large constants.

* Fix: Structure alignment is now respected for substructures.

* X16: Fixed the address of `vera_dc_hscale_hstop` register (#54) (thanks to @Kobrasadetin).

* Fixed evaluation of more complex boolean expressions (#56).

* Fixed accesses to volatile variables.

* 8080/Z80: Optimization improvements. 

* 6809: Optimization improvements.

* Various Atari improvements (#60, #63, #65) (thanks to @zbyti).

* New Atari examples (thanks to @zbyti).

## 0.3.18 (2020-04-08)

* Support for Motorola 6809 (complete, but still experimental).

* Preliminary support for Tandy Color Computer running RS-DOS.

* Preliminary support for 16K cartridges for Tandy Color Computer.

* Added support for modifying large variables via pointers.

* Added the ability to declare structure alignment.

* `for` loops over arrays.

* Allowed defining custom text encodings.
**Potentially breaking change!**
There are no built-in encodings now, the include path needs to contain the necessary encodings.

* Fixed encodings: 
`apple2`, `atasciiscr`, `iso_de`, `iso_no`, `iso_se`, 
`koi7n2`, `msx_jp`, 
`oldpet`, `origpet`,  `petscii`, `petsciijp`, `petscr`, `petscrjp`, 
`zx80`.

* Added encodings:
`apple2c`, `apple2e`, `apple2gs`,
`coco`, `cocoscr`,
`cpc_da`, `cpc_en`, `cpc_es`, `cpc_fr`,
`cp437`, `cp850`, `cp851`, `cp852`, `cp855`, `cp858`, `cp866`, 
`cp1250`, `cp1251`, `cp1252`,
`ebcdic`,
`galaksija`,
`iso8859_1`, `iso8859_2`, `iso8859_3`, `iso8859_4`, `iso8859_5`,
`iso8859_7`, `iso8859_9`, `iso8859_10`, `iso8859_13`, `iso8859_14`, `iso8859_16`,
`kamenicky`,
`koi8e`, `koi8f`, `koi8r`, `koi8ru`, `koi8t`, `koi8u`,
`mazovia`, `pcw`,
`pokemon1en`, `pokemon1es`, `pokemon1fr`, `pokemon1jp`.

* Added `ENCODING_NOLOWER` preprocessor feature.

* Fixed raw views of typed pointers.

* Fixed dead code elimination (#51). 

* **Potentially breaking change!** Changed default encoding for CPC to `cpc_en`.

* **Potentially breaking change!** Changed the type of `mouse_lbm` and `mouse_rbm` to `bool`. Added `mouse_mbm`

* **Potentially breaking change!** Renamed the `x_coord` module to `coord`. Added the `y_coord` type and `TALLSCREEN` preprocessor feature.

* Added `pscrstr2word` function.

* Labels with fixed addresses are now exported to the label file (#49).

* Fixed address of the VIC 20 volume register (#52) (thanks to @nippur72).

* Fixed and improved stdlib optimizations.

* Allow importing modules from subdirectories.

* Allow placing platform definitions in a dedicated subdirectory.

* Allow using Batch files with the `-r` option.

* Improved error reporting for constants used before their definitions.

* Improved typo hints.

* Typo hints for non-ASCII characters.

* Z80: Intel syntax for all Z80 instructions, based on Digital Research's Z80.LIB.

* Commander X16: Updated to support VERA 0.9 and the new joystick API. Added mouse support.

* 6502: Optimization improvements:

    * Fixed index register optimization regressions.
    
    * Small array optimizations are now available for more operations.
    
    * Index calculations for arrays of structs with sizes divisible by an even power of two are now sometimes optimized.
    
    * Redundant index calculations are now removed.

## 0.3.16 (2020-04-08)

* Language improvements:

    * Added compile-time evaluation for user-defined functions.
    
    * Added `breakpoint` macro (#44).

    * **Potentially breaking change!** Added `min`, `max` and `if` compile-time functions.
    
    * Added experimental `signed16` and `unsigned16` types.
    
    * Added length-prefixed strings (Pascal strings).  
    
    * Improved operator support for word-sized arguments (#24, #25).
    
    * **Potentially breaking change!** Various macros improvements, including the requirement of parameter types matching exactly (#23, #39, #40).

* Compiler improvements:

    * 6809 improvements (no full support yet).
    
    * Added warnings for calling from one segment to another overlapping one.
    
    * 6502: Fixed undocumented mnemonics.
    
    * Create output directories when needed (#21).
    
    * Allow defining different output formats for different segments when outputting one file per segment.
    
    * Fixed multiple optimization bugs (#32, #38, #41, #46 and others) – thanks to @agg23 for detailed bug reports!
    
    * 6502: Fix boolean arrays and pointers (#28).

    * Fixed and improved memset-like loops (#47).
    
    * Minor improvements to inline assembly.
    
    * Improvements to constant evaluation, including more arithmetic operators.
    
    * **Potentially breaking change!** Detect overflowing constants, like `const byte x = 256`.
    
    * Optimization improvements.
    
    * 6502: Memory allocation improvements for pointers.
    
    * Support for MkImg (tool for creating BBC Micro disk images) and multiple output files on BBC Micro.
    
    * Other minor fixes.

* Library improvements:

    * Added `putsigned16` function.
    
    * Added `pstring` module and `putpstr` function.  

    * Various improvements to the C64 libraries (thanks to @bsutherland).
    
    * Added detection for various PET variants and implemented `readkey` for PET.
    
    * Implemented `readkey` and `readline` for Apple II.
    
    * Changed the default load address for BBC Micro.

    * Multiple fixes to the `string`, `scrstring` and `encconv` modules.
    
    * Other minor fixes.

* Other changes:

    * Created a pure-Millfork test suite.

    * Updated to Scala 2.12.11.

## 0.3.14 (2019-12-03)

* Full assembly support for HuC6280.

* Improvements to subroutine extraction; it's now much faster, less buggy and actually effective.

* 8080: function parameters can now be optimized to registers.

* Fixed error messages about invalid function flags.

* 6502: Fixed compilation of interrupt routines.

* 65C02: Fixed fatal bugs related to some 65C02 subtypes.

* Other bug fixes.

## 0.3.12 (2019-11-06)

* **Breaking change!**
The `petscr`, `petscrjp` and `atasciiscr` encodings now use $E0, $E0 and $DB respectively as their string terminator.

* **Potentially breaking change!**
Changed the identifiers for various subtypes of the 65C02 processors.

* Added `nullchar_scr` and `NULLCHAR_SCR` 

* Added the `scrstring` module

* Added `strz_from_screencode`, `strz_to_screencode`, `strzpaste` and `scrstrzpaste` functions

* Added the ability to convert from booleans to integers

* Fixed the string escape bug

* Unary minus and other parser improvements

* Better error reporting

* Other bugfixes

## 0.3.10 (2019-10-24)

* Preliminary support for the CPU from ZX Spectrum Next.

* Added the ability to define custom segment layouts.

* Hardware definitions for Atari 8-bit (thanks to @FreddyOffenga)

* Fixed BLL headers for Atari Lynx (#7).

* Some improvements for Commander X16 (with help from @Ambez05)

* Specified consistent behaviour of `for` loops (#8)

* Fixed bugs with constant booleans.

* Fixed bugs with arithmetic promotions of signed values.

* Fixed a bug with unused global variable elimination (#10).

* Fixed a bug with variable overlapping (#11).

* 6502: Fixed a bug with flow analysis during optimizations.

* 6502: Fixed a bug with certain 16-bit additions.

* 8080: Fixed and optimized 16-bit comparisons.

* 8080: Optimized some library functions.

* Optimized certain byte comparisons and pointer indexing.

* 6502: Optimized certain boolean conversions and some small array accesses.

* Unused built-in functions are now removed more accurately.

* Updated to Scala 2.12.10.

* Added another NES example (thanks to @Garydos).

## 0.3.8 (2019-06-21)

* `sizeof` now supports arrays.

* Added `-R` option for specifying extra commandline parameters for emulators.

* Added full 16-bit multiplication and unsigned division. 

* Added preliminary support for Atari Lynx (thanks to @Nullious).

* Added preliminary support for EasyFlash.

* Added preliminary support for Commander X16.

* Allowed defining custom output padding byte value.

* Allowed passing non-decimal numbers to the `-D` option.

* Added the `encconv` module.

* Added `nullchar` constant as the null terminator for strings and `NULLCHAR` feature to define its value.

* Added `vectrex`, `msx_br`, `koi7n2`, `iso15`, `zx80` and `zx81` text encodings.

* `reset_joy` is now defined to always reset all joypad state variables.

* Fixed arithmetic promotion bugs for signed values.

* Fixed parsing of `zp_bytes` in platform definitions.

* Fixed several serious bugs related to cartridge-based targets.

* 6502: Few minor optimization improvements.

* 6502: Inlining improvements.

## 0.3.6 (2019-08-05)

* **Breaking change!**
The `petscii` encoding now uses the $C0-$DE range for uppercase characters instead of $60-$7E.
This matches both the CC65 behaviour and the return values from `readkey()`.

* Added support for the Japanese version of Commodore 64.

* Added `segment` block statement.

* Added goto.

* Added `bool` type.

* Added function pointers – so far quite limited.

* Added arrays of elements of size greater than byte.

* Added `.length` for large arrays and `.lastindex` for numerically indexed arrays.

* New text encodings: `petjp`, `petscrjp`, `msx_intl`, `msx_jp`, `msx_ru`.

* Improved passing of register parameters to assembly functions.

* Enabled declaring multiple variables in one line.

* Fixed detection of duplicate names in declarations.

* 6502: Fixed expressions of form `p[i] <<= 1`.

* 6502: Fixed variable bit shifting. 

* 6502: Fixed word division by a divisor larger than 127.

* 6502: Fixed byte multiplication.

* 8080/Z80: Fixed byte division.

* Fixed many optimization bugs:

    * incorrect removal of unused local variables;
    
    * incorrect removal of certain type casts;
    
    * broken parameter passing to tail calls;

    * 6502: miscompilation when using the zeropage pseudoregister;

    * 6502: stack overflow when inlining local variables into registers;
    
    * 6502: not setting the high byte to 0 when optimizing word multiplication by 0

    * 8080/Z80: compiler crash when compiling conditions;
 
    * 8080/Z80: miscompilation of code after `if` statements;
 
    * 8080/Z80: miscompilation near multiplication;

    * Z80: miscompilation when using stack variables.

* Other fixes and improvements.

## 0.3.4 (2019-07-01)

* Preliminary experimental Game Boy support.

* Preliminary MSX support.

* Super experimental and very incomplete Intel 8086 support via 8080-to-8086 translation.

* Support for Intel 8085, together with illegal instructions.

* More label file formats.

* Added `memory_barrier` macro.

* Added `keyboard` module.

* Added `random` module.

* Added `init_rw_memory` module for cartridge targets.

* **Breaking change!** Preinitialized writable arrays on cartridge targets
can no longer be read before an explicit call to `init_rw_memory`, either add the call or make the arrays const.

* Added `ensure_mixedcase` function and `oldpet` and `origpet` text encodings.

* Added `MILLFORK_VERSION` preprocessor parameter.

* Added structs and unions.

* Added unsigned byte division and modulo. 

* Pointers can now be allocated anywhere.

* Pointers can now be typed.

* Added `nullptr`.

* You can now take a pointer to a stack variable.

* Arrays can now have elements of types other than `byte` (still limited in size to 1 byte though) and be built out of struct literals.

* Arrays can now be constant.

* Arrays can now be local.

* Added hint for identifiers with typos.

* Aliases now also support subfields.

* Short functions can be now defined using expression syntax.

* On most Commodore targets printing double quote characters doesn't toggle the quotation mode anymore. 

* **Potentially breaking change!** Commodore 128 target no longer defines `CBM_64` feature.

* Fixed the NES examples and the `nes_joy` module. 

* 6502 targets can now define free zeropage bytes, not only pointers.

* 6502: Fixed optimizations using index registers.

* 6502: Fixed optimizations of comparisons.

* 6502: Fixed optimizations near `for` loops over in-place lists.

* Preprocessor: Added `#use A=B` directive

* Preprocessor: Added `if` function

* Preprocessor: Added `#define` directive.

* Fixed volatile-related bugs.

* Fixed optimizations removing jumps to jumps.

* Fixed optimizations removing pointless stores to local variables.

* Fixed name clashes when passing parameters to functions and macros.

* Fixed `#use` not accessing all preprocessor parameters.

* Fixed `#pragma` not respecting `#if`.

* Fixed nested `#if`s.

* Fixed `@long` and `@long_be` array filters.

* Fixed for-each loops with non-constant arrays.

* 8080 and LR35902: Fixed inlining of byte-sized variables into registers.

* 8080 and LR35902: fixed large stack variables.

* Other bug fixes.

* Optimization improvements.

## 0.3.2 (2018-12-28)

* Almost complete support for the Zilog Z80, Intel 8080 and Sharp LR35902 microprocessors.

* A very incomplete support for NEC PC-88, ZX Spectrum, CP/M and Amstrad CPC.

* Unified the syntax of commandline switches.

* Automatic detection of the standard include path.

* Added aliases.

* Added enumeration types.

* Added preprocessor.

* Added `for` loops over enum types and in-place lists

* Added `align` keyword for choosing data and code alignment.

* Added original line numbers in assembler output.

* Added `sizeof` operator.

* Added preliminary support for `volatile` keyword.

* Added multiplication of a 16-bit number by an unsigned 8-bit number.

* Added more warnings.

* Automatic selection of text encoding based on target platform.

* Text literals can be now used as expressions of type `pointer`.

* Extra `z` at the name of the encoding means that the string is zero-terminated.

* **Potentially breaking change!** No longer allowed to define things with names that are keywords or builtins.

* **Potentially breaking change!** Curly braces in text literals are now used for escape sequences.

* **Potentially breaking change!** Changed the `c64_basic` module.

* **Potentially breaking change!** `scr` now refers to the default screencodes as defined for the platform.
Code that uses both a custom platform definition and the `scr` encoding needs attention
(either change `scr` to `petscr` or add `screen_encoding=petscr` in the platform definition file).

* **Potentially breaking change!** Platform definitions now need appropriate feature definitions. 
Code that uses a custom platform definitions will cause extra warnings until fixed.

* Software variable stack for 6502.

* Other optimizations and fixes for stack variables.

* Various code deduplication optimizations.

* Fixed emitting constant decimal expressions.

* Fixed decimal subtraction.

* Fixed signed comparison.

* Fixed `for`...`downto` loops.

* Fixed invalid optimization of loads before `BEQ` and similar

* Fixed miscompiled `nonet` calls.

* Parser performance improvement.

* Standard libraries improvements.

* Other improvements.

## 0.3.0 (2018-06-25)

* Finally faster than C. 

* Licenced the standard library more permissively.

* Preliminary Atari 2600, BBC Micro and LUnix support.

* Added array initialization syntax with `for`.

* Added multiple new text codecs.

* Added character literals.

* Added 24-bit `farword` type. 

* Special array layouts, e.g. `@word`. 

* Fixed invalid offsets for branching instructions.

* Fixed incorrectly overlapping local variables.

* Fixed broken `downto` loops.

* Fixed broken comparisons between variables of different sizes.

* Fixed several other bugs.

* Tons of optimizer improvements.

* Other improvements.

## 0.2.2 (2018-03-19)

* Allowed adding constant words to variable bytes without the zeropage pseudoregister.

* `-fzp-register` is now enabled by default, as the documentation has already been saying.

* Allowed more kinds of constants within variable and array initializers.

* Fixed several bugs.

* Other improvements.

## 0.2 (2018-03-17)

* **Breaking change!** Renamed `inline` to `macro`.

* **Breaking change!** Added support for memory segments. Changed the platform definition file syntax.

* Added preliminary support for 65CE02, HuC6280 and 65816 processors.

* Added support for Famicom/NES and C64 with SuperCPU.

* Added new `-O1` optimization preset; old `-O1` became `-O2`, old `-O2` became `-O3` and so on.

* Added command line options for controlling the size-speed trade-offs.

* Added support for parameters for macros written in Millfork.

* Enabled calling macros with index expression parameters.

* Enabled calling macros from assembly.

* Added optimizer hints: `inline`, `noinline`, `register`.
 
* Added command line flags `--size`, `--fast`, `--blast-processing`.
 
* Removed command line flag `--detailed-flow`. 
Detailed flow analysis was slow, broken, hard to maintain, and didn't even help that much.

* Added `*'=`, `nonet`, `hi` and `lo` operators.

* Added support for zeropage pseudoregisters, allowing for some operators work with more types of operands. 

* Added return dispatch statements.

* Added `break` and `continue` statements.

* Allowed the `else if` combination (i.e. the `else` branch doesn't have to be in braces if it's just a single `if`-`else` statement).

* Added octal and quaternary literals.

* Fixed several allocation bugs.

* Fixed several optimization bugs.

* Fixed several C64 and C16 library bugs.

* Fixed several other bugs.

* Other improvements.

## 0.1 (2018-01-24)

* Initial numbered version.
