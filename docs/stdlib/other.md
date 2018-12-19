[< back to index](../index.md)

## stdio

The `stdio` module automatically imports the `string` and `err` modules.  
It requires an implementation of `void putchar(byte a)` and therefore works only on targets with console output.

#### `void putstr(pointer str, byte len)`

Prints a string of length `len` located at address `str`.  

#### `void putstrz(pointer str)`

Prints a null-terminated string located at address `str`.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).


## err

#### `enum error_number`

Standard error codes. All codes other than `err_ok` suggest that the last operation failed.

#### `error_number errno`

The result of the last operation.
