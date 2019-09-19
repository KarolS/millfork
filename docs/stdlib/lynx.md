[< back to index](../doc_index.md)

# Atari Lynx-oriented modules

## atari_lynx_hardware

The `atari_lynx_hardware` module is imported automatically on NES targets.

It also implements a joystick API compatible with the `joy` module.

TODO

#### `void lynx_init()`

TODO

#### `void lynx_wait_suzy()`

TODO

#### `alias input_a = input_btn`

1 if A button pressed, 0 if not pressed.

#### `byte input_b`

1 if B button pressed, 0 if not pressed.

#### `void read_joy()`

Reads the joypad.

#### `void lynx_reset_joy()`
#### `alias reset_joy = lynx_reset_joy!`

Resets the state variables.
