[< back to index](../index.md)

## string

The `string` module automatically imports the `err` module.  

#### `byte strzlen(pointer str)`

Calculates the length of a null-terminated string.  
If the string is longer than 255 bytes, then the behaviour is undefined (might even crash).

#### `sbyte strzcmp(pointer str1, pointer str2)`

Compares two null-terminated strings. Returns 0 if equal, non-0 if not equal.
If any of the strings is longer than 255 bytes, then the behaviour is undefined (might even crash).

#### `void strzcopy(pointer dest, pointer src)`

Copies the source null-terminated string into the destination buffer.
If the source string is longer than 255 bytes, then the behaviour is undefined (might even crash).

#### `word strz2word(pointer str)`

Converts a null-terminated string to a number.
Sets `errno`.

#### `void strzappend(pointer buffer, pointer str)`
#### `void strzappendchar(pointer buffer, byte char)`

Modifies the given null-terminated buffer by appending a null-terminated string or a single character respectively.
