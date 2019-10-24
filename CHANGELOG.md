# Change log

## 0.3.10

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

## 0.3.8

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

## 0.3.6

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

## 0.3.4

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

## 0.3.2

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

## 0.3.0

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

## 0.2.2

* Allowed adding constant words to variable bytes without the zeropage pseudoregister.

* `-fzp-register` is now enabled by default, as the documentation has already been saying.

* Allowed more kinds of constants within variable and array initializers.

* Fixed several bugs.

* Other improvements.

## 0.2

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

## 0.1

* Initial numbered version.