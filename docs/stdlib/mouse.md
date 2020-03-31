[< back to index](../doc_index.md)

## mouse

The `mouse` module automatically imports the `x_coord` module.

The module contains global variables representing the state of the mouse.
If the program is not using any mouse driver, the state of these variables is undefined.

To actually use this module, an appropriate mouse module must be used, such as [`c1531`](./c64.md).

#### `x_coord mouse_x`

Mouse X position.

#### `byte mouse_y`

Mouse Y position.

#### `byte mouse_lbm`

1 if the left mouse button is being pressed, 0 otherwise

#### `byte mouse_rbm`

1 if the right mouse button is being pressed, 0 otherwise

## x_coord

#### `alias x_coord`

The type for representing horizontal screen coordinates.
It's `byte` if the screen is 256 pixels wide or less,
or `word` if the screen is more that 256 pixels wide.

## null_mouse_default

This module set the default mouse to null mouse.
The null mouse has no button pressed and the cursos is fixed at coordinates (0,0). 

#### `void read_mouse()`
