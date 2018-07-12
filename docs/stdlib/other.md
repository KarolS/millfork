[< back to index](../index.md)

## `stdio` module

The `stdio` module automatically imports the `string` module.  
It requires an implementation of `void putchar(byte a)` and therefore works only on targets with console output.

#### `void putstr(pointer str, byte len)`

Prints a string of length `len` located at address `str`.  

#### `void putstrz(pointer str)`

Prints a null-terminated string located at address `str`.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

## `string` module

#### `byte strzlen(pointer str)`

Calculates the length of a null-terminated string.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

## `mouse` module

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