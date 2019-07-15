[< back to index](../doc_index.md)

## keyboard

The `keyboard` module provides support for reading keypresses from the keyboard.
Not supported on all targets.

#### `byte readkey()`

Waits for and reads a single keypress.

The returning values may vary between platforms:

* letters may be uppercase or lowercase

* modifier keys may be applied or ignored

Available for:
Commodore 64 (requires KERNAL),
Commodore 16 or Plus/4 (requires KERNAL),
Commodore 128 (requires KERNAL),
VIC 20 (requires KERNAL),
Atari,
Amstrad CPC,
ZX Spectrum,
NEC PC-88.

#### `const byte KEY_ENTER`

Key code for the Enter/Return key. Usually 13, but not always.

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
