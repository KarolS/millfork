[< back to index](../doc_index.md)

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

* On Atari computers, reads the POKEY random register.

* On Z80, reads the refresh register.

* On all other targets, sets the seed to 1.
