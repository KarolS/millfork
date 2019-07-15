[< back to index](../doc_index.md)

# Game Boy–oriented modules

## gb_hardware

The `gb_hardware` module is imported automatically on Game Boy targets.

TODO

## gb_header_small

The `gb_header_small` module is imported automatically on small Game Boy targets.
It contains the default header for 32K Game Boy programs.  

## gb_joy

Provides an interface for reading joypads that is compatible with the `joy` module.

#### `alias input_a = input_btn`

1 if A button pressed, 0 id not pressed.

#### `byte input_b`

1 if B button pressed, 0 id not pressed.

#### `byte input_select`

1 if Select button pressed, 0 id not pressed.

#### `byte input_start`

1 if Start button pressed, 0 id not pressed.

#### `void read_joy()`

Reads the joypad.
