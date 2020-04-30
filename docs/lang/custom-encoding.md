[< back to index](../doc_index.md)

### Defining custom encodings

Every encoding is defined in an `.tbl` file with an appropriate name.
The file is looked up in the directories on the include path, first directly, then in the `encoding` subdirectory.

The file is a UTF-8 text file, with each line having a specific meaning.
In the specifications below, `<>` are not to be meant literally:

* lines starting with `#`, `;` or `//` are comments.

* `ALIAS=<another encoding name>` defines this encoding to be an alias for another encoding.
No other lines are allowed in the file.

* `NAME=<name>` defines the name for this encoding. Required.

* `BUILTIN=<internal name>` defines this encoding to be a UTF-based encoding.
`<internal name>` may be one of `UTF-8`, `UTF-16LE`, `UTF-16BE`.
If this directive is present, the only other allowed directive in the file is the `NAME` directive.

* `EOT=<xx>` where `<xx>` are two hex digits, defines the string terminator byte.
Required, unless `BUILTIN` is present.
There have to be two digits, `EOT=0` is invalid.

* lines like `<xx>=<c>` where `<xx>` are two hex digits
and `<c>` is either a **non-whitespace** character or a **BMP** Unicode codepoint written as `U+xxxx`,
define the byte `<xx>` to correspond to character `<c>`.
There have to be two digits, `0=@` is invalid.

* lines like `<xx>-<xx>=<c><c><c><c>` where `<c>` is repeated an appropriate number of times
define characters for multiple byte values.
In this kind of lines, characters cannot be represented as Unicode codepoints.

* lines like `<c>=<xx>`, `<c>=<xx><xx>` etc.
define secondary or alternate characters that are going to be represented as one or more bytes.
There have to be two digits, `@=0` is invalid.
Problematic characters (space, `=`, `#`, `;`) can be written as Unicode codepoints `U+xxxx`.

* a line like `a-z=<xx>` is equivalent to lines `a=<xx>`, `b=<xx+$01>` all the way to `z=<xx+$19>`.

* a line like `KATAKANA=>DECOMPOSE` means that katakana characters with dakuten or handakuten
should be split into the base character and the standalone dakuten/handakuten.

* similarly with `HIRAGANA=>DECOMPOSE`.

* lines like `{<escape code>}=<xx>`, `{<escape code>}=<xx><xx>` etc.
define escape codes. It's a good practice to define these when possible:
`{q}`, `{apos}`, `{n}`, `{lbrace}`, `{rbrace}`, 
`{yen}`, `{pound}`, `{cent}`, `{euro}`, `{copy}`, `{pi}`,
`{nbsp}`, `{shy}`.

