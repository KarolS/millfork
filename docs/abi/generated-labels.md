[< back to index](../doc_index.md)

# Guide to generated label names

Many Millfork constructs generate labels. 
Knowing what they mean can be useful when reading the generated assembly code. 

Every generated label is of form `.xx__11111` 
where `11111` is a sequential number and `xx` is the type:

* `ah` – optimized addition of carry

* `an` – logical conjunction short-circuiting

* `bc` – array bounds checking (`-fbounds-checking`)

* `bo` – boolean type conversions

* `c8` – constant `#8` for `BIT` when immediate addressing is not available

* `co` – greater-than comparison

* `cp` – equality comparison for larger types

* `dd` – labels renamed by code deduplication

* `de` – decrement for larger types

* `do` – beginning of a `do-while` statement

* `ds` – decimal right shift operation

* `dv` – division and modulo operations

* `el` – beginning of the "else" block in an `if` statement

* `ew` – end of a `while` statement

* `fi` – end of an `if` statement

* `fe` – body of an `for` statement over a list

* `fo` – certain optimized `for` loops

* `he` – beginning of the body of a `while` statement

* `in` – increment for larger types

* `is` – optimized addition of carry using undocumented instructions

* `lj` – extra labels generated when converting invalid short jumps to long jumps

* `me` – start of a `for` loop doing bulk memory operations

* `ms` – bulk memory operations

* `no` – nonet to word extension caused by the `nonet` operator

* `od` – end of  a `do-while` statement

* `or` – logical alternative short-circuiting

* `sx` – sign extension, from a smaller signed type to a larger type

* `th` – beginning of the "then" block in an `if` statement

* `to` – end of a `for-to` loop

* `ur` – a copy due to loop unrolling

* `wh` – beginning of a `while` statement

* `xc` – automatically extracted subroutine of commonly repeating code

