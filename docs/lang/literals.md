# Literals and initializers

## Numeric literals

Decimal: `1`, `10`

Binary: `%0101`, `0b101001`

Quaternary: `0q2131`

Octal: `0o172`

Hexadecimal: `$D323`, `0x2a2`

## String literals

String literals are surrounded with double quotes and followed by the name of the encoding:

    "this is a string" ascii

Characters between the quotes are interpreted literally, 
there are no ways to escape special characters or quotes.

In some encodings, multiple characters are mapped to the same byte value,
for compatibility with multiple variants.

Currently available encodings:

* `ascii` – standard ASCII

* `pet` or `petscii` – PETSCII (ASCII-like character set used by Commodore machines)

* `scr` – Commodore screencodes

* `apple2` – Apple II charset ($A0–$FE)

* `bbc` – BBC Micro and ZX Spectrum character set

* `jis` – JIS X 0201

* `iso_de`, `iso_no`, `iso_se`, `iso_yu` – various variants of ISO/IEC-646
 
* `iso_dk`, `iso_fi` – aliases for `iso_no` and `iso_se` respectively

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