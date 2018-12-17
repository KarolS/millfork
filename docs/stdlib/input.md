[< back to index](../index.md)

## joy

The module contains global variables representing the state of the one-button joystick.
If the program is not using any joystick driver, the state of these variables is undefined.

#### `sbyte input_dx`

Horizontal joystick movement. 1 if right, -1 if left, 0 if neither.

#### `sbyte input_dy`

Vertical joystick movement. 1 if right, -1 if left, 0 if neither.

#### `byte input_btn`

1 if main button pressed, 0 id not pressed.

#### `void reset_joy()`

Resets the state variables.
For platforms with more than one button, this resets only the main button state.

## null_joy_default

This module set the default joystick to no joystick. 

#### `alias read_joy`

## mouse

The `mouse` module automatically imports the `x_coord` module.

The module contains global variables representing the state of the mouse.
If the program is not using any mouse driver, the state of these variables is undefined.

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

This module set the default joystick to no joystick. 

#### `void read_mouse()`
