[< back to index](../doc_index.md)

# Text encodings ans escape sequences

### Text encoding list

* `default` – default console encoding (can be omitted)

* `scr` – default screencodes
(usually the same as `default`, a notable exception are the Commodore computers)

* `ascii` – standard ASCII

* `pet` or `petscii` – PETSCII (ASCII-like character set used by Commodore machines from VIC-20 onward)

* `petjp` or `petsciijp` – PETSCII as used on Japanese versions of Commodore 64

* `origpet` or `origpetscii` – old PETSCII (Commodore PET with original ROMs)

* `oldpet` or `oldpetscii` – old PETSCII (Commodore PET with newer ROMs)

* `cbmscr` or `petscr` – Commodore screencodes

* `cbmscrjp` or `petscrjp` – Commodore screencodes as used on Japanese versions of Commodore 64

* `apple2` – Apple II charset ($A0–$DF)

* `bbc` – BBC Micro character set

* `sinclair` – ZX Spectrum character set

* `zx80` – ZX80 character set

* `zx81` – ZX81 character set

* `jis` or `jisx` – JIS X 0201

* `iso_de`, `iso_no`, `iso_se`, `iso_yu` – various variants of ISO/IEC-646
 
* `iso_dk`, `iso_fi` – aliases for `iso_no` and `iso_se` respectively

* `iso15` – ISO 8859-15

* `latin0`, `latin9`, `iso8859_15` – aliases for `iso15`

* `msx_intl`, `msx_jp`, `msx_ru`, `msx_br` – MSX character encoding, International, Japanese, Russian and Brazilian respectively

* `msx_us`, `msx_uk`, `msx_fr`, `msx_de` – aliases for `msx_intl`
 
* `atascii` or `atari` – ATASCII as seen on Atari 8-bit computers
 
* `atasciiscr` or `atariscr` – screencodes used by Atari 8-bit computers

* `koi7n2` or `short_koi` – KOI-7 N2

* `vectrex` – built-in Vectrex font

* `utf8` – UTF-8

* `utf16be`, `utf16le` – UTF-16BE and UTF-16LE

When programming for Commodore,
use `pet` for strings you're printing using standard I/O routines
and `petscr` for strings you're copying to screen memory directly.

### Escape sequences

Escape sequences allow for including characters in the string literals that would be otherwise impossible to type.

Some escape sequences may expand to multiple characters. For example, in several encodings `{n}` expands to `{x0D}{x0A}`.

##### Available everywhere

* `{q}` – double quote symbol

* `{x00}`–`{xff}` – a character of the given hexadecimal value

* `{copyright_year}` – this expands to the current year in digits

* `{program_name}` – this expands to the name of the output file without the file extension

* `{program_name_upper}` – the same, but uppercased

* `{nullchar}` – the null terminator for strings (`"{nullchar}"` is equivalent to `""z`)

##### Available only in some encodings

* `{apos}` – apostrophe/single quote (available everywhere except for `zx80` and `zx81`)

* `{n}` – new line

* `{b}` – backspace

* `{lbrace}`, `{rbrace}` – opening and closing curly brace (only in encodings that support braces)

* `{up}`, `{down}`, `{left}`, `{right}` – control codes for moving the cursor

* `{white}`, `{black}`, `{red}`, `{green}`, `{blue}`, `{cyan}`, `{yellow}`, `{purple}` – 
control codes for changing the text color

* `{bgwhite}`, `{bgblack}`, `{bgred}`, `{bggreen}`, `{bgblue}`, `{bgcyan}`, `{bgyellow}`, `{bgpurple}` – 
control codes for changing the text background color

* `{reverse}`, `{reverseoff}` – inverted mode on/off

* `{yen}`, `{pound}`, `{cent}`, `{euro}`, `{copy}` – yen symbol, pound symbol, cent symbol, euro symbol, copyright symbol

* `{u0000}`–`{u1fffff}` – Unicode codepoint (available in UTF encodings only)

##### Character availability

Encoding | lowercase letters | backslash | currencies | intl | card suits  
---------|-------------------|-----------|------------|------|-----------  
`pet`,              | yes¹ | no  | £    | none      | yes¹  
`origpet`           | yes¹ | yes |      | none      | yes¹  
`oldpet`            | yes² | yes |      | none      | yes²  
`petscr`            | yes¹ | no  | £    | none      | yes¹  
`petjp`             | no   | no  | ¥    | katakana³ | yes³  
`petscrjp`          | no   | no  | ¥    | katakana³ | yes³  
`sinclair`, `bbc`   | yes  | yes | £    | none      | no  
`zx80`, `zx81`      | no   | no  | £    | none      | no  
`apple2`            | no   | yes |      | none      | no  
`atascii`           | yes  | yes |      | none      | yes  
`atasciiscr`        | yes  | yes |      | none      | yes  
`jis`               | yes  | no  | ¥    | both kana | no  
`iso15`             | yes  | yes | €¢£¥ | Western   | no   
`msx_intl`,`msx_br` | yes  | yes | ¢£¥  | Western   | yes   
`msx_jp`            | yes  | no  | ¥    | katakana  | yes   
`msx_ru`            | yes  | yes |      | Russian⁴  | yes   
`koi7n2`            | no   | yes |      | Russian⁵  | no   
`vectrex`           | no   | yes |      | none      | no   
`utf*`              | yes  | yes | all  | all       | yes  
all the rest        | yes  | yes |      | none      | no  
  
1. `pet`, `origpet` and `petscr` cannot display card suit symbols and lowercase letters at the same time.
Card suit symbols are only available in graphics mode,
in which lowercase letters are displayed as uppercase and uppercase letters are displayed as symbols. 

2.  `oldpet` cannot display card suit symbols and lowercase letters at the same time.
Card suit symbols are only available in graphics mode, in which lowercase letters are displayed as symbols. 

3. `petjp` and `petscrjp` cannot display card suit symbols and katakana at the same time.
Card suit symbols are only available in graphics mode, in which katakana is displayed as symbols. 

4. Letter **Ё** and uppercase **Ъ** are not available.

5. Only uppercase. Letters **Ё** and **Ъ** are not available.

If the encoding does not support lowercase letters (e.g. `apple2`, `petjp`, `petscrjp`, `koi7n2`, `vectrex`),
then text and character literals containing lowercase letters are automatically converted to uppercase. 
Only unaccented Latin and Cyrillic letters will be converted as such.
Accented Latin letters will not be converted and will fail to compile without `-flenient-encoding`.     
To detect if your default encoding does not support lowercase letters, test `'A' == 'a'`.

##### Escape sequence availability

Encoding | new line | braces | backspace | cursor movement | text colour | reverse | background colour  
---------|----------|--------|-----------|-----------------|-------------|---------|------------------  
`pet`,`petjp`       | yes | no  | no  | yes | yes | yes | no  
`origpet`           | yes | no  | no  | yes | no  | yes | no  
`oldpet`            | yes | no  | no  | yes | no  | yes | no  
`petscr`, `petscrjp`| no  | no  | no  | no  | no  | no  | no  
`sinclair`          | yes | yes | no  | yes | yes | yes | yes  
`zx80`,`zx81`       | yes | no  | yes | yes | no  | no  | no  
`ascii`, `iso_*`    | yes | yes | yes | no  | no  | no  | no  
`iso15`             | yes | yes | yes | no  | no  | no  | no  
`apple2`            | no  | yes | no  | no  | no  | no  | no  
`atascii`           | yes | no  | yes | yes | no  | no  | no  
`atasciiscr`        | no  | no  | no  | no  | no  | no  | no  
`msx_*`             | yes | yes | yes | yes | no  | no  | no  
`koi7n2`            | yes | no  | yes | no  | no  | no  | no  
`vectrex`           | no  | no  | no  | no  | no  | no  | no  
`utf*`              | yes | yes | yes | no  | no  | no  | no  
all the rest        | yes | yes | no  | no  | no  | no  | no
