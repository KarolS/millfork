[< back to index](../doc_index.md)

## stdlib

The `stdlib` module is automatically imported on most targets.

#### `macro asm void poke(word const addr, byte a)`

Stores a byte at given constant address. Will not be optimized away by the optimizer.

#### `macro asm byte peek(word const addr)`

Reads a byte from given constant address. Will not be optimized away by the optimizer.

#### `macro asm void disable_irq()`

Disables interrupts.

#### `macro asm void enable_irq()`

Enables interrupts.

#### `byte hi_nibble_to_hex(byte a)`

Returns an ASCII representation of the upper nibble of the given byte.

#### `byte lo_nibble_to_hex(byte a)`

Returns an ASCII representation of the lower nibble of the given byte.

#### `macro asm void panic()`

Crashes the program.
