[< back to index](../index.md)

## stdio

The `stdio` module automatically imports the `string` module.  
It requires an implementation of `void putchar(byte a)` and therefore works only on targets with console output.

#### `void putstr(pointer str, byte len)`

Prints a string of length `len` located at address `str`.  

#### `void putstrz(pointer str)`

Prints a null-terminated string located at address `str`.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

## string

#### `byte strzlen(pointer str)`

Calculates the length of a null-terminated string.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).
