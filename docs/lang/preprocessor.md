[< back to index](../index.md)

# Preprocessor

The Millfork preprocessor does 2 things:

* filters lines in the input file according to current target's features

* injects the target's feature values as constants into the current file

Despite its similarity to the C preprocessor, it's much more restricted in its power:

* no file inclusion

* no macros 

* separate namespaces for the preprocessor and the language (you need to use `#use` to use a preprocessor constant in the code)

### Defining feature values

Feature values are defined in the `[define]` section of the platform definition file.  
Each value is a signed 64-bit integer number.

Example:

    [define]
    WIDESCREEN=1
    
You can also define feature values using the `-D` command line option.

### Built-in features

* `ARCH_6502` – 1 if compiling for 6502, 0 otherwise 

* `ARCH_Z80` – 1 if compiling for Z80, 0 otherwise 

### Commonly used features

* `WIDESCREEN` – 1 if the horizontal screen resolution, ignoring borders, is greater than 256, 0 otherwise

* `CBM` – 1 if the target is an 8-bit Commodore computer, 0 otherwise

* `CBM_64` – 1 if the target is an 8-bit Commodore computer compatible with Commodore 64, 0 otherwise

* `KEYBOARD` – 1 if the target has a keyboard, 0 otherwise

* `JOYSTICKS` – the maximum number of joysticks using standard hardware configurations, may be 0

* `HAS_BITMAP_MODE` – 1 if the target has a display mode with every pixel addressable, 0 otherwise

### Built-in preprocessor functions and operators

The `defined` function returns 1 if the feature is defined, 0 otherwise.  
All the other functions and operators treat undefined features as if they were defined as 0. 

TODO
`not`, `lo`, `hi`, `+`, `-`, `*`, `|`, `&`, `^`, `||`, `&&`, `<<`, `>>`,`==`, `!=`, `>`, `>=`, `<`, `<=`

The following Millfork operators and functions are not available in the preprocessor:  
`+'`, `-'`, `*'`, `<<'`, `>>'`, `:`, `>>>>`, `nonet`, all the assignment operators

### `#if/#elseif/#else/#endif`

    #if <expr>
    #elseif <expr>
    #else
    #endif

TODO

### `#fatal/#error/#warn/#info`

    #error fatal error message
    #error error message
    #warn warning message
    #info informational message
    
Emits a diagnostic message.

`#fatal` interrupts the compilation immediately.  
`#error` causes an error, but the compilation will continue.  
`#warn` emits a warning. It may be treated as an error depending on compilation options.  
`#info` emits a benign diagnostic message.


### `#use`

Exports a feature value under its name to the parser.
The parser will substitute every use of that name as a variable or constant 
with the numeric value of the feature.
The substitution will happen only within the current file.
To use such value in other files, consider this:

    #use WIDESCREEN
    const byte is_widescreen = WIDESCREEN

