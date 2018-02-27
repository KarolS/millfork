# Literals and initializers

## Numeric literals

Decimal: `1`, `10`

Binary: `%0101`, `0b101001`

Hexadecimal: `$D323`, `0x2a2`

## String literals

String literals are surrounded with double quotes and followed by the name of the encoding:

    "this is a string" ascii

Characters between the quotes are interpreted literally, 
there are no ways to escape special characters or quotes.

Currently available encodings:

* `ascii` – standard ASCII

* `pet` or `petscii` – PETSCII (ASCII-like character set used by Commodore machines)

* `scr` – Commodore screencodes

When programming for Commodore,
use `pet` for strings you're printing using standard I/O routines
and `scr` for strings you're copying to screen memory directly.


## Array initialisers 

An array is initialized with either a string literal,
or a list of byte literals and strings, surrounded by brackets:

    array a = [1, 2]
    array b = "----" scr
    array c = ["hello world!" ascii, 13]

Trailing commas (`[1, 2,]`) are not allowed.