[< back to index](../doc_index.md)

## keyboard

The `keyboard` module provides support for reading keypresses from the keyboard.
Not supported on all targets.

For reading entire lines of text from the keyboard, see the `readline` and `readword` functions 
in [the frequently provided definitions](./frequent.md).

#### `byte readkey()`

Waits for and reads a single keypress.

The returning values may vary between platforms:

* letters may be uppercase or lowercase

* modifier keys may be applied or ignored

Available for:
Commodore 64 (requires KERNAL),
Commodore 16 or Plus/4 (requires KERNAL),
Commodore 128 (requires KERNAL),
Commodore PET (requires KERNAL),
VIC 20 (requires KERNAL),
Atari,
Amstrad CPC,
ZX Spectrum,
NEC PC-88,
Tandy Color Computer.

#### `const byte KEY_ENTER`

Key code for the Enter/Return key. Usually 13, but not always.
