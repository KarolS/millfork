[< back to index](../doc_index.md)

## encconv

The `encconv` module provides functions for character set conversions.

All the functions are defined only for the characters that are valid in both input and output encoding.
Unsupported characters may give arbitrary results.
The unsupported characters are not guaranteed to roundtrip.

Some encodings (e.g. PETSCII) allow for multiple encoding of the same character.
For the input, all encodings are equivalent.
For the output, the canonical encoding is preferred.

Characters that are present in the input encoding,
but are encoded as multiple bytes in the output encoding, are not supported.

#### byte to_screencode(byte)

Converts a byte from the default encoding to the screen encoding.

If both encodings contain the character `¤`, then `to_screencode('¤') == '¤'scr`.

Available only if one of the following is true:

* the default encoding and the screen encoding are the same

* the default encoding is `petscii`, the screen encoding is `petscr`, and the platform is 6502-based

* the default encoding is `petsciijp`, the screen encoding is `petscrjp`, and the platform is 6502-based

* the default encoding is `atascii`, the screen encoding is `atasciiscr`, and the platform is 6502-based

You can test for the availability of this function using the `ENCCONV_SUPPORTED` preprocessor feature.

#### byte from_screencode(byte)

Converts a byte from the screen encoding to the default encoding.

If both encodings contain the character `¤`, then `from_screencode('¤'scr) == '¤'`.

Available only if `to_screencode` is available.

#### void strz_to_screencode(pointer)

Destructively converts a null-terminated string from the `default` encoding into the `scr` encoding. 

Available only if `to_screencode` is available.

#### void strz_from_screencode(pointer)

Destructively converts a null-terminated string from the `scr` encoding into the `default` encoding. 

Available only if `from_screencode` is available.

#### byte petscii_to_petscr(byte)

Converts a byte from PETSCII to a CBM screencode.  
Works also for the variants used on the Japanese version of C64.  
Control characters are converted to reverse characters, the same as in the standard quote mode.

Available only on 6502-based platforms.

#### byte petscr_to_petscii(byte)

Converts a byte from a CBM screencode to PETSCII.  
Works also for the variants used on the Japanese version of C64.  
Reverse characters are interpreted as control characters or as non-reverse characters.

Available only on 6502-based platforms.

#### byte atascii_to_atasciiscr(byte)

Converts a byte from ATASCII to a Atari screencode.  
Control characters <$80 are converted to the graphical characters that share the ATASCII code.  
Control characters ≥$80 are not supported.  

Available only on 6502-based platforms.

#### byte atasciiscr_to_atascii(byte)

Converts a byte from a Atari screencode to ATASCII.  
Characters that share their ATASCII code with control characters are supported,
but they require to be escaped with $1B to be printed.  
Reverse characters are interpreted as non-reverse characters.

Available only on 6502-based platforms.

