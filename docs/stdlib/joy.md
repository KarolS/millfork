[< back to index](../doc_index.md)

## joy

The module contains global variables representing the state of the one-button joystick.
If the program is not using any joystick driver, the state of these variables is undefined.

To actually use this module, an appropriate joystick module must be used,
such as  [`c64_joy`](./c64.md), [`nes_joy`](./nes.md), [`gb_joy`](./gb.md) or [`x16_joy`](./x16.md).

#### `sbyte input_dx`

Horizontal joystick movement. 1 if right, -1 if left, 0 if neither.

#### `sbyte input_dy`

Vertical joystick movement. 1 if down, -1 if up, 0 if neither.

#### `byte input_btn`

1 if main button pressed, 0 if not pressed.

#### `void reset_joy()`

Resets the state variables.  
The default implementation resets only the main button.  
May be overridden by a strong alias on some platforms that have more buttons.

## null_joy_default

This module set the default joystick to no joystick. 

#### `alias read_joy`

A reserved name for reading the default joystick.
