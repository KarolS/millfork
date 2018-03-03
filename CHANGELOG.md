# Change log

## Current version

* **Breaking change!** Renamed `inline` to `macro`.

* Added preliminary support for 65CE02, HuC6280 and 65816 processors.

* Added new `-O1` optimization preset; old `-O1` became `-O2`, old `-O2` became `-O3` and so on.

* Added support for parameters for macros written in Millfork.

* Enabled calling macros with index expression parameters.

* Added optimizer hints: `inline`, `noinline`, `register`.
 
* Added command line flags `--size`, `--fast`, `--blast-processing`.

* Added `*'=` and `<<<<` operators.

* Added return dispatch statements.

* Added octal and quaternary literals.

* Fixed several optimization bugs.

* Fixed several C64 library bugs.

* Fixed several other bugs.

* Other improvements.

## 0.1

* Initial numbered version.