[< back to index](../doc_index.md)

## stdlib

The `stdlib` module is automatically imported on most targets.

#### `macro asm void poke(word const addr, byte register(a) value)`

Stores a byte at given constant address. Will not be optimized away by the optimizer.

#### `macro asm byte peek(word const addr)`

Reads a byte from given constant address. Will not be optimized away by the optimizer.

#### `macro asm void disable_irq()`

Disables interrupts.

#### `macro asm void enable_irq()`

Enables interrupts.

#### `byte hi_nibble_to_hex(byte register(a) value)`

Returns an ASCII representation of the upper nibble of the given byte.

#### `byte lo_nibble_to_hex(byte register(a) value)`

Returns an ASCII representation of the lower nibble of the given byte.

#### `macro asm void panic()`

Crashes the program.

## Standard macros available without any import

#### `macro void memory_barrier()`

Informs the optimizer that at this point arbitrary memory has been accessed and either read or written by an external device.
The optimizer should not optimize any memory accesses across that macro.

Available for: all targets.

#### `macro void breakpoint()`

If the `-fbreakpoints` option is selected (default), then it emits a memory barrier,
and also outputs a breakpoint to the label file (if the format of the label file allows it).

If the `-fno-breakpoints` option is selected, then it does nothing.

Available for: all targets.
