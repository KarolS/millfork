[< back to index](../doc_index.md)

## mouse

The `mouse` module automatically imports the `coord` module.

The module contains global variables representing the state of the mouse.
If the program is not using any mouse driver, the state of these variables is undefined.

To actually use this module, an appropriate mouse module must be used, such as [`c1531`](./c64.md).

#### `x_coord mouse_x`

Mouse X position.

#### `y_coord mouse_y`

Mouse Y position.

#### `bool mouse_lbm`

`true` if the left mouse button is being pressed, `false` otherwise

#### `byte mouse_mbm`

`true` if the middle mouse button is being pressed, `false` otherwise.
Available only if `USE_MOUSE_MBM` is set and non-zero.

#### `byte mouse_rbm`

`true` if the right mouse button is being pressed, `false` otherwise

## coord

#### `alias x_coord`

The type for representing horizontal screen coordinates.
It's `byte` if the screen is 256 pixels wide or less,
or `word` if the screen is more that 256 pixels wide.

#### `alias y_coord`

The type for representing vertical screen coordinates.
It's `byte` if the screen is 256 pixels tall or less,
or `word` if the screen is more that 256 pixels tall.

## null_mouse_default

This module set the default mouse to null mouse.
The null mouse has no button pressed and the cursor is fixed at coordinates (0,0). 

#### `void read_mouse()`
