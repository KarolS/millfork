# Change log

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