[< back to index](../index.md)

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

* `jis` or `jisx` – JIS X 0201

* `iso_de`, `iso_no`, `iso_se`, `iso_yu` – various variants of ISO/IEC-646
 
* `iso_dk`, `iso_fi` – aliases for `iso_no` and `iso_se` respectively

When programming for Commodore,
use `pet` for strings you're printing using standard I/O routines
and `scr` for strings you're copying to screen memory directly.

## Character literals

Character literals are surrounded by single quotes and followed by the name of the encoding: 

    'x' ascii

From the type system point of view, they are constants of type byte.

## Array initialisers 

An array is initialized with either:

* a string literal

* a `file` expression

* a `for`-style expression

* a format, followed by an array initializer:

   *   `@word` (=`@word_le`): for every term of the array initializer, emit two bytes, first being the low byte of the value, second being the high byte:      
       `@word [$1122]` is equivalent to `[$22, $11]`
   
   *   `@word_be` – like the above, but opposite:  
       `@word_be [$1122]` is equivalent to `[$11, $22]`
   

* a list of byte literals and/or other array initializers, surrounded by brackets:


    array a = [1, 2]
    array b = "----" scr
    array c = ["hello world!" ascii, 13]
    array d = file("d.bin")
    array e = file("d.bin", 128, 256)
    array f = for x,0,until,8 [x * 3 + 5]  // equivalent to [5, 8, 11, 14, 17, 20, 23, 26]

Trailing commas (`[1, 2,]`) are not allowed.

The parameters for `file` are: file path, optional start offset, optional length
(start offset and length have to be either both present or both absent).

The `for`-style expression has a variable, a starting index, a direction, a final index, 
and a parameterizable array initializer.
The initializer is repeated for every value of the variable in the given range.
