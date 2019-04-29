# Change log

## Current version

* Preliminary experimental Game Boy support.

* Added `memory_barrier` macro.

* Added `random` module.

* Added `ensure_mixedcase` function and `oldpet` and `origpet` text encodings.

* Added `MILLFORK_VERSION` preprocessor parameter.

* Added structs and unions.

* Pointers can now be allocated anywhere.

* Pointers can now be typed.

* Added `nullptr`.

* Arrays can now have elements of types other than `byte` (still limited in size to 1 byte though).

* Arrays can now be constant.

* Added hint for identifiers with typos.

* Aliases now also support subfields.

* Short functions can be now defined using expression syntax.

* 6502: Fixed optimizations using index registers.

* Fixed volatile-related bugs.

* Fixed optimizations removing jumps to jumps.

* Fixed optimizations removing pointless stores to local variables.

* Fixed `#use` not accessing all preprocessor parameters.

* Fixed `#pragma` not respecting `#if`.

* 8080 and LR35902: fixed large stack variables.

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