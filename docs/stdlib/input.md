[< back to index](../doc_index.md)

## joy

The module contains global variables representing the state of the one-button joystick.
If the program is not using any joystick driver, the state of these variables is undefined.

To actually use this module, an appropriate joystick module must be used, such as `c64_joy`, `nes_joy` or `gb_joy`.

#### `sbyte input_dx`

Horizontal joystick movement. 1 if right, -1 if left, 0 if neither.

#### `sbyte input_dy`

Vertical joystick movement. 1 if down, -1 if up, 0 if neither.

#### `byte input_btn`

1 if main button pressed, 0 id not pressed.

#### `void reset_joy()`

Resets the state variables.  
The default implementation resets only the main button.  
May be overridden by a strong alias on some platforms that have more buttons.

## null_joy_default

This module set the default joystick to no joystick. 

#### `alias read_joy`

A reserved name for reading the default joystick.

## mouse

The `mouse` module automatically imports the `x_coord` module.

The module contains global variables representing the state of the mouse.
If the program is not using any mouse driver, the state of these variables is undefined.

To actually use this module, an appropriate mouse module must be used, such as `c1531`.

#### `x_coord mouse_x`

Mouse X position.

#### `byte mouse_y`

Mouse Y position.

#### `byte mouse_lbm`

1 if the left mouse button is being pressed, 0 otherwise

#### `byte mouse_rbm`

1 if the right mouse button is being pressed, 0 otherwise

## `x_coord` module

#### `alias x_coord`

The type for representing horizontal screen coordinates.
It's `byte` if the screen is 256 pixels wide or less,
or `word` if the screen is more that 256 pixels wide.

## null_mouse_default

This module set the default mouse to no mouse. 

#### `void read_mouse()`
