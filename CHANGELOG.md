# Change log

## Current version

* **Breaking change!** Renamed `inline` to `macro`.

* Added preliminary support for 65CE02, HuC6280 and 65816 processors.

* Added new `-O1` optimization preset; old `-O1` became `-O2`, old `-O2` became `-O3` and so on.

* Added support for parameters for macros written in Millfork.

* Enabled calling macros with index expression parameters.

* Enabled calling macros from assembly.

* Added optimizer hints: `inline`, `noinline`, `register`.
 
* Added command line flags `--size`, `--fast`, `--blast-processing`.
 
* Removed command line flag `--detailed-flow`. 
Detailed flow analysis was slow, broken, hard to maintain, and didn't even help that much.

* Added `*'=` and `nonet` operators.

* Added support for zeropage pseudoregisters, allowing for some operators work with more types of operands. 

* Added return dispatch statements.

* Added `break` and `continue` statements.

* Allowed the `else if` combination (i.e. the `else` branch doesn't have to be in braces if it's just a single `if`-`else` statement).

* Added octal and quaternary literals.

* Fixed several allocation bugs.

* Fixed several optimization bugs.

* Fixed several C64 library bugs.

* Fixed several other bugs.

* Other improvements.

## 0.1

* Initial numbered version.