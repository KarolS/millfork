[< back to index](../doc_index.md)

Definitions on the following list are frequently provided by the default automatically-imported modules. 

However, as they are not the part of the standard library, they might not be available on all targets:

#### `void init_rw_memory()`

Initializes all writable arrays and variables with their initial values.

If the preprocessor feature `INIT_RW_MEMORY` is defined and non-zero,
then `init_rw_memory` is available and should be called before accessing any preinitialized writable object.

If the preprocessor feature `INIT_RW_MEMORY` is not defined or is zero,
then `init_rw_memory` is not available.

#### `void putchar(byte char)`

Prints a single character.

Available for: all computer targets.  
Uses ROM routines, so requires the appropriate ROM to be enabled if applicable.

Note that this function may obey typical platform idiosyncrasies, for example:

* on Commodore PET targets the quote character toggles the quotation mode

* printing past the end of line might insert a blank line below the current one

* printing past the end of the screen might ask the user to confirm scrolling

The exact behaviour is platform-dependent.
Future library versions will strive to eliminate those issues.

#### `void new_line()`

Moves the cursor to the next line.

Available for: all computer targets.  
Uses ROM routines, so requires the appropriate ROM to be enabled if applicable.

#### `pointer readline()`

Reads a line from the console and returns a pointer to a null-terminated string.
The string is valid only until next read from the console.

Available for:
ZX Spectrum,
NEC PC-88,
MSX,
Commodore 64 with `c64_basic` module (requires KERNAL and BASIC),
Commodore 16 or Plus/4 with `c264_basic` module (requires KERNAL and BASIC).

#### `word readword()`

Reads a 16-bit unsigned integer from the console.

Available for:
ZX Spectrum,
NEC PC-88,
MSX,
Commodore 64 with `c64_basic` module (requires KERNAL and BASIC),
Commodore 16 or Plus/4 with `c264_basic` module (requires KERNAL and BASIC).

#### `void bell()`

Beeps.

Available for: Apple 2, ZX Spectrum.
Uses ROM routines, so requires the appropriate ROM to be enabled if applicable.

#### `void set_bg_color(byte color)`

Sets the screen background color.

Available for: C64, VIC-20, C64, C128, C264 series.

#### `void set_border(byte color)`

Sets the screen border color.

Available for: VIC-20, C64, C128, C264 series, ZX Spectrum.

#### `const byte black, white, red, green, blue, cyan, purple, yellow`

Various colour constants.

Available for: VIC-20, C64, C128, C264 series, ZX Spectrum.

#### `macro void memory_barrier()`

Informs the optimizer that at this point arbitrary memory has been accessed and either read or written by an external device.
The optimizer should not optimize any memory accesses across that macro.

Available for: all targets.


