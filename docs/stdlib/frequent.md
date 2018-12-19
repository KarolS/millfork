[< back to index](../index.md)

Definitions on the following list are frequently provided by the default automatically-imported modules. 

However, as they are not the part of the standard library, they might not be available on all targets:

#### `void putchar(byte char)`

Prints a single character.

Available for: all computer targets.

#### `void new_line()`

Moves the cursor to the next line.

Available for: all computer targets.

#### `pointer readline()`

Reads a line from the console and returns a pointer to a null-terminated string.
The string is valid only until next read from the console.

Available for:
ZX Spectrum,
NEC PC-88,
Commodore 64 with `c64_basic` module,
Commodore 16 or Plus/4 with `c264_basic` module.

#### `word readword()`

Reads a 16-bit unsigned integer from the console.

Available for:
ZX Spectrum,
NEC PC-88,
Commodore 64 with `c64_basic` module,
Commodore 16 or Plus/4 with `c264_basic` module.

#### `void bell()`

Beeps.

Available for: Apple 2, ZX Spectrum.

#### `void set_bg_color(byte color)`

Sets the screen background color.

Available for: C64, VIC-20, C64, C264 series.

#### `void set_border(byte color)`

Sets the screen border color.

Available for: VIC-20, C64, C264 series, ZX Spectrum.

#### `const byte black, white, red, green, blue, cyan, purple, yellow`

Various colour constants.

Available for: VIC-20, C64, C264 series, ZX Spectrum.

