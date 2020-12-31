[< back to index](../doc_index.md)

# Literals and initializers

## Numeric literals

Decimal: `1`, `10`

Binary: `%0101`, `0b101001`

Quaternary: `0q2131`

Octal: `0o172`

Hexadecimal: `$D323`, `0x2a2`

When using Intel syntax for inline assembly, another hexadecimal syntax is available: `0D323H`, `2a2h`.
It is not allowed in any other places.

The type of a literal is the smallest type of undefined signedness
that can fit either the unsigned or signed representation of the value:
`200` is a `byte`, `4000` is a `word`, `75000` is an `int24` etc.

However, padding the literal to the left with zeroes changes the type
to the smallest type that can fit the smallest number with the same number of digits and without padding.
For example, `0002` is of type `word`, as 1000 does not fit in one byte.

## String literals

String literals can be used as either array initializers or expressions of type `pointer`.

String literals are equivalent to constant arrays. Writing to them via their pointer is undefined behaviour.

If a string literal is used as an expression, then the text data will be located in the default code segment,
regardless of which code segment the current function is located it. This may be subject to change in future releases.

String literals are surrounded with double quotes and optionally followed by the name of the encoding:

    "this is a string" ascii
    "this is also a string"

If there is no encoding name specified, then the `default` encoding is used. 
Two encoding names are special and refer to platform-specific encodings:
`default` and `scr`.

## Zero-terminated strings

You can also append `z` to the name of the encoding to make the string zero-terminated.
This means that the string will have a string terminator appended, usually a single byte.
The exact value of that byte is encoding-dependent:
* in the `vectrex` encoding it's 128,
* in the `zx80` encoding it's 1,
* in the `zx81` encoding it's 11,
* in the `petscr` and `petscrjp` encodings it's 224,
* in the `atascii` encoding it's 219,
* in the `utf16be` and `utf16le` encodings it's exceptionally two bytes: 0, 0
* in other encodings it's 0 (this may be a subject to change in future versions).

        "this is a zero-terminated string" asciiz
        "this is also a zero-terminated string"z

The byte constant `nullchar` is defined to be equal to the string terminator in the `default` encoding (or, in other words, to `'{nullchar}'`)
and the byte constant `nullchar_scr` is defined to be equal to the string terminator in the `scr` encoding (`'{nullchar}'scr`).

You can override the values for `nullchar` and `nullchar_scr`
by defining preprocessor features `NULLCHAR` and `NULLCHAR_SCR` respectively. 

Warning: If you define UTF-16 to be you default or screen encoding, you will encounter several problems:

* `nullchar` and `nullchar_scr` will still be bytes, equal to zero.
* the `string` module in the Millfork standard library will not work correctly

## Length-prefixed strings (Pascal strings)

You can also prepend `p` to the name of the encoding to make the string length-prefixed.

The length is measured in bytes and doesn't include the zero terminator, if present.
In all encodings except for UTF-16 the prefix takes one byte,
which means that length-prefixed strings cannot be longer than 255 bytes.
 
In case of UTF-16, the length prefix contains the number of code units,
so the number of bytes divided by two,
which allows for strings of practically unlimited length.
The length is stores as two bytes and is always little endian,
even in case of the `utf16be` encoding or a big-endian processor.

        "this is a Pascal string" pascii
        "this is also a Pascal string"p
        "this is a zero-terminated Pascal string"pz

Note: A string that's both length-prefixed and zero-terminated does not count as a normal zero-terminated string!
To pass it to a function that expects a zero-terminated string, add 1 (or, in case of UTF-16, 2):

    pointer p
    p = "test"pz
    // putstrz(p)  // won't work correctly
    putstrz(p + 1) // ok

## Escape sequences and miscellaneous compatibility issues

Most characters between the quotes are interpreted literally.
To allow characters that cannot be inserted normally,
each encoding may define escape sequences.
Every encoding is guaranteed to support at least 
`{q}` for double quote 
and `{apos}` for single quote/apostrophe.

The number of bytes used to represent given characters may differ from the number of the characters.
For example, the `petjp`, `msx_jp` and `jis` encodings represent ポ as two separate characters, and therefore two bytes.

For the list of all text encodings and escape sequences, see [this page](./text.md).

In some encodings, multiple characters are mapped to the same byte value,
for compatibility with multiple variants.

If the characters in the literal cannot be encoded in particular encoding, an error is raised.
However, if the command-line option `-flenient-encoding` is used,
then literals using `default` and `scr` encodings replace unsupported characters with supported ones, 
skip unsupported escape sequences, and a warning is issued.
For example, if `-flenient-encoding` is enabled, then a literal `"£¥↑ž©ß"` is equivalent to:

* `"£Y↑z(C)ss"` if the default encoding is `pet`

* `"£Y↑z©ss"` if the default encoding is `bbc`

* `"?Y^z(C)ss"` if the default encoding is `ascii`

* `"?Y^ž(C)ss"` if the default encoding is `iso_yu`

* `"?Y^z(C)ß"` if the default encoding is `iso_de`

* `"?¥^z(C)ss"` if the default encoding is `jisx`

* `"£¥^z(C)β"` if the default encoding is `msx_intl`

Note that the final length of the string may vary.

## Character literals

Character literals are surrounded by single quotes and optionally followed by the name of the encoding: 

    'x' ascii
    'W'

Character literals have to be separated from preceding operators with whitespace:

    a='a'    // wrong 
    a = 'a'  // ok 

From the type system point of view, they are constants of type byte.

If the character cannot be represented as one byte, an error is raised.

For the list of all text encodings and escape sequences, see [this page](./text.md).

If the characters in the literal cannot be encoded in particular encoding, an error is raised.
However, if the command-line option `-flenient-encoding` is used,
then literals using `default` and `scr` encodings replace unsupported characters with supported ones.
If the replacement is one character long, only a warning is issued, otherwise an error is raised.

## Struct constructors

You can create a constant of a given struct type by listing constant values of fields as arguments:

    struct point { word x, word y }
    point(5,6)
    

## Array initializers 

An array is initialized with either:

* (only byte arrays) a string literal

* (only byte arrays) a `file` expression

* a `for`-style expression

* (only byte arrays) a format, followed by an array initializer:

   *   `@word_le`: for every term of the array initializer, emit two bytes, first being the low byte of the value, second being the high byte:      
       `@word_le [$1122]` is equivalent to `[$22, $11]`
   
   *   `@word_be` – like the above, but opposite:  
       `@word_be [$1122]` is equivalent to `[$11, $22]`
       
   *   `@word`: equivalent to `@word_le` on little-endian architectures and `@word_be` on big-endian architectures
       
   *   `@long`, `@long_le`, `@long_be`: similar, but with four bytes      
       `@long_le [$11223344]` is equivalent to `[$44, $33, $22, $11]`  
       `@long_be [$11223344]` is equivalent to `[$11, $22, $33, $44]`
       
   *   `@struct`: every term of the initializer is interpreted as a struct constructor (see below) 
   and treated as a list of bytes with no padding   
       `@struct [s(1, 2)]` is equivalent to `[1, 2]` when `struct s {byte x, byte y}` is defined  
       `@struct [s2(1, 2), s2(3, 4)]` is equivalent to `[1, 0, 2, 0, 3, 0, 4, 0]` on little-endian machines when `struct s2 {word x, word y}` is defined  

* a list of literals and/or other array initializers, surrounded by brackets:

        array a = [1, 2]
        array b = "----" scr
        array c = ["hello world!" ascii, 13]
        array d = file("d.bin")
        array d1 = file("d.bin", 128)
        array e = file("d.bin", 128, 256)
        array f = for x,0,until,8 [x * 3 + 5]  // equivalent to [5, 8, 11, 14, 17, 20, 23, 26]
        array(point) g = [point(2,3), point(5,6)]
        array(point) i = for x,0,until,100 [point(x, x+1)]

Trailing commas (`[1, 2,]`) are not allowed.

String literals are laid out in the arrays as-is, flat.
To have an array of pointers to strings, wrap each string in `pointer(...)`:

    // a.length = 12; identical to [$48, $45, $4C, $4C, $4F, 0, $57, $4F, $52, $4C, $44, 0]
    array a = [ "hello"z, "world"z ] 
    // b.length = 2
    array(pointer) b = [ pointer("hello"z), pointer("world"z) ]

The parameters for `file` are: file path, optional start offset, optional length
(if only two parameters are present, then the second one is assumed to be the start offset).
The `file` expression is expanded at the compile time to an array of bytes equal to the bytes contained in the file.
If the start offset is present, then that many bytes at the start of the file are skipped.
If the length is present, then only that many bytes are taken, otherwise, all bytes until the end of the file are taken. 

The `for`-style expression has a variable, a starting index, a direction, a final index, 
and a parameterizable array initializer.
The initializer is repeated for every value of the variable in the given range.

Struct constructors look like a function call, where the callee name is the name of the struct type 
and the parameters are the values of fields in the order of declaration.  
Fields of arithmetic, pointer and enum types are declared using normal expressions.  
Fields of struct types are declared using struct constructors.
Fields of union types cannot be declared.

What might be useful is the fact that the compiler allows for certain built-in functions
in constant expressions only:

* `sin(x, n)` – returns _n_·**sin**(*x*π/128)

* `cos(x, n)` – returns _n_·**cos**(*x*π/128)

* `tan(x, n)` – returns _n_·**tan**(*x*π/128)

* `min(x,...)` – returns the smallest argument

* `max(x,...)` – returns the largest argument

