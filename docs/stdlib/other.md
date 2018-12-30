[< back to index](../index.md)

## stdio

The `stdio` module automatically imports the `string` and `err` modules.  
It requires an implementation of `void putchar(byte a)` and therefore works only on targets with console output.

#### `void putstr(pointer str, byte len)`

Prints a string of length `len` located at address `str`.  

#### `void putstrz(pointer str)`

Prints a null-terminated string located at address `str`.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

#### `void ensure_mixedcase()`

On targets that have separate all-caps and mixed-case modes (like most Commodore machines), switches to the mixed-case mode.


## err

#### `enum error_number`

Standard error codes. All codes other than `err_ok` suggest that the last operation failed.

#### `error_number errno`

The result of the last operation.


## random

A simple and reasonably fast random number generator. Not suitable for cryptographic or simulation uses.

#### `word rand_seed`

Random number generator state.

#### `byte rand()`

Get a random byte (0–255) and update the state of the generator.

#### `void init_rand_seed()`

Initializes the random number generator state.
This may take a long time, even several frames, depending on the target.

Current implementation:  

* On C64, spends two frames reading noise data from the SID chip.

* On Z80, reads the refresh register.

* On all other targets, sets the seed to 1.
