[< back to index](../doc_index.md)

# Magic suffixes

## Byte-related suffixes

These suffixes can be only applied to arithmetic or pointer variables: 

* `.lo` – the least significant byte of a two-byte variable (word, pointer) (use `lo(_)` for arbitrary expressions)

* `.hi` – the most significant byte of a two-byte variable (word, pointer) (use `hi(_)` for arbitrary expressions)

* `.loword` – the least significant word of a three- or four-byte variable

* `.hiword` – the most significant word of a three- or four-byte variable

* `.b0`, `.b1` etc. – the given byte of the multi-byte arithmetic variable, with `.b0` being the least significant byte

## Pointer-related suffixes:

These suffixes can be applied to variables, arrays, functions or pointable expressions (sometimes called _lvalues_): 

* `.addr` – returns address of the object (type `pointer`) (constant unless on Lunix)

* `.rawaddr` – returns the raw address constant of the object (type `pointer`, the same as `.addr` unless on Lunix, guaranteed to be constant)

* `.pointer` – returns the typed pointer to the object

This suffix is available only on expressions that have a type of a typed pointer:

* `.raw` – a view of the pointer as a raw pointer

## Segment-related suffixes

These suffixes can be applied to variables, arrays, or functions:

* `.segment.bank` (or `.segment` for short) – returns the bank number of the segment the object is in 

* `.segment.start` – returns the start address of the segment the object is in 

* `.segment.codeend` – returns the last address of code in the segment the object is in 

* `.segment.datastart` – returns the start address of data in the segment the object is in 

* `.segment.heapstart` – returns the start address of uninitialized data in the segment the object is in 

* `.segment.end` – returns the last address of the segment the object is in

* `.segment.fill` – returns the byte value used to fill gaps and other unused space in the segment the object is in

See also [the list of predefined constants](./predefined_constants.md).
 