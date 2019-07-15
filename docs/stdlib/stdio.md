[< back to index](../doc_index.md)

## stdio

The `stdio` module automatically imports the `string` and `err` modules.  
It requires an implementation of `void putchar(byte a)` and therefore works only on targets with console output.
On targets with idiosyncratic behaviour of `putchar`, functions in this module inherit that behaviour.

#### `void putstr(pointer str, byte len)`

Prints a string of length `len` located at address `str`.  

#### `void putstrz(pointer str)`

Prints a null-terminated string located at address `str`.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

#### `void putword(word w)`

Prints the decimal representation of the 16-bit unsigned integer `w`.

#### `void ensure_mixedcase()`

On targets that have separate all-caps and mixed-case modes (like most Commodore machines), switches to the mixed-case mode.

