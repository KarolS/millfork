[< back to index](../index.md)

## stdio

The `stdio` module automatically imports the `string` and `err` modules.  
It requires an implementation of `void putchar(byte a)` and therefore works only on targets with console output.

#### `void putstr(pointer str, byte len)`

Prints a string of length `len` located at address `str`.  

Note that both this function and `putstrz` obey typical platform idiosyncrasies,
for example on CBM targets the quote character will toggle the quotation mode. This may be subject to change.

#### `void putstrz(pointer str)`

Prints a null-terminated string located at address `str`.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

#### `void putword(word w)`

Prints the decimal representation of the 16-bit unsigned integer `w`.

#### `void ensure_mixedcase()`

On targets that have separate all-caps and mixed-case modes (like most Commodore machines), switches to the mixed-case mode.

