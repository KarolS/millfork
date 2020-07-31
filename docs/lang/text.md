[< back to index](../doc_index.md)

# Text encodings and escape sequences

### Defining custom encodings

Every platform is defined in an `.tbl` file with an appropriate name.
The file is looked up in the directories on the include path, first directly, then in the `encoding` subdirectory.

TODO: document the file format.

### Text encoding list

* `default` – default console encoding (can be omitted)

* `scr` – default screencodes
(usually the same as `default`, a notable exception are the Commodore computers)

* `ascii` – standard ASCII

* `petscii` or `pet` – PETSCII (ASCII-like character set used by Commodore machines from VIC-20 onward)

* `petsciijp` or `petjp` – PETSCII as used on Japanese versions of Commodore 64

* `origpetscii` or `origpet` – old PETSCII (Commodore PET with original ROMs)

* `oldpetscii` or `oldpet` – old PETSCII (Commodore PET with newer ROMs)

* `cbmscr` or `petscr` – Commodore screencodes

* `cbmscrjp` or `petscrjp` – Commodore screencodes as used on Japanese versions of Commodore 64

* `apple2` – original Apple II charset ($A0–$DF)

* `apple2e` – Apple IIe charset

* `apple2c` – alternative Apple IIc charset

* `apple2gs` – Apple IIgs charset

* `bbc` – BBC Micro character set

* `sinclair` – ZX Spectrum character set

* `zx80` – ZX80 character set

* `zx81` – ZX81 character set

* `jis` or `jisx` – JIS X 0201

* `iso_de`, `iso_no`, `iso_se`, `iso_yu` – various variants of ISO/IEC-646
 
    * `iso_dk`, `iso_fi` – aliases for `iso_no` and `iso_se` respectively

* `iso8859_1`, `iso8859_2`, `iso8859_3`,
`iso8859_4`, `iso8859_5`, `iso8859_7`,
`iso8859_9`, `iso8859_10`, `iso8859_13`,
`iso8859_14`, `iso8859_15`, `iso8859_13` – 
ISO 8859-1, ISO 8859-2, ISO 8859-3,
ISO 8859-4, ISO 8859-5, ISO 8859-7,
ISO 8859-9, ISO 8859-10, ISO 8859-13,
ISO 8859-14, ISO 8859-15, ISO 8859-16,

    * `iso1`, `latin1` – aliases for `iso8859_1`
    * `iso2`, `latin2` – aliases for `iso8859_2`
    * `iso3`, `latin3` – aliases for `iso8859_3`
    * `iso4`, `latin4` – aliases for `iso8859_4`
    * `iso5` – alias for `iso8859_5`
    * `iso7` – alias for `iso8859_7`
    * `iso9`, `latin5`, – aliases for `iso8859_9`
    * `iso10`, `latin6` – aliases for `iso8859_10`
    * `iso13`, `latin7` – aliases for `iso8859_13`
    * `iso14`, `latin8` – aliases for `iso8859_14`
    * `iso_15`, `latin9`, `latin0` – aliases for `iso8859_15`
    * `iso16`, `latin10` – aliases for `iso8859_16`

* `cp437`, `cp850`, `cp851`, `cp852`, `cp855`, `cp858`, `cp866` –
DOS codepages 437, 850, 851, 852, 855, 858, 866

* `mazovia` – Mazovia encoding

* `kamenicky` – Kamenický encoding

* `cp1250`, `cp1251`, `cp1252` – Windows codepages 1250, 1251, 1252

* `msx_intl`, `msx_jp`, `msx_ru`, `msx_br` – MSX character encoding, International, Japanese, Russian and Brazilian respectively

    * `msx_us`, `msx_uk`, `msx_fr`, `msx_de` – aliases for `msx_intl`
    
* `cpc_en`, `cpc_fr`, `cpc_es`, `cpc_da` – Amstrad CPC character encoding, English, French, Spanish and Danish respectively

* `pcw` or `amstrad_cpm` – Amstrad CP/M encoding, the US variant (language 0), as used on PCW machines

* `pokemon1en`, `pokemon1jp`, `pokemon1es`, `pokemon1fr` – text encodings used in 1st generation Pokémon games,
English, Japanese, Spanish/Italian and French/German respectively

    * `pokemon1it`, `pokemon1de` – aliases for `pokemon1es` and `pokemon1fr` respectively
 
* `atascii` or `atari` – ATASCII as seen on Atari 8-bit computers
 
* `atasciiscr` or `atariscr` – screencodes used by Atari 8-bit computers

* `koi7n2` or `short_koi` – KOI-7 N2

* `koi8r`, `koi8u`, `koi8ru`, `koi8e`, `koi8f`, `koi8t` – various variants of KOI-8

* `vectrex` – built-in Vectrex font

* `galaksija` – text encoding used on Galaksija computers

* `coco` – text encoding used on Tandy Color Computer

* `cocoscr` – Tandy Color Computer screencodes

* `ebcdic` – EBCDIC codepage 037 (partial coverage)

* `utf8` – UTF-8

* `utf16be`, `utf16le` – UTF-16BE and UTF-16LE

When programming for Commodore,
use `petscii` for strings you're printing using standard I/O routines
and `petsciiscr` for strings you're copying to screen memory directly.

When programming for Atari,
use `atascii` for strings you're printing using standard I/O routines
and `atasciiscr` for strings you're copying to screen memory directly.

### Escape sequences

Escape sequences allow for including characters in the string literals that would be otherwise impossible to type.

Some escape sequences may expand to multiple characters. For example, in several encodings `{n}` expands to `{x0D}{x0A}`.

##### Available everywhere

* `{x00}`–`{xff}` – a character of the given hexadecimal value

* `{copyright_year}` – this expands to the current year in digits

* `{program_name}` – this expands to the name of the output file without the file extension

* `{program_name_upper}` – the same, but uppercased

* `{nullchar}` – the null terminator for strings (`"{nullchar}"` is equivalent to `""z`).  
The exact value of `{nullchar}` is encoding-dependent:

    * in the `vectrex` encoding it's `{x80}`,
    * in the `zx80` encoding it's `{x01}`,
    * in the `zx81` encoding it's `{x0b}`,
    * in the `petscr` and `petscrjp` encodings it's `{xe0}`,
    * in the `atasciiscr` encoding it's `{xdb}`,
    * in the `pokemon1*` encodings it's `{x50}`,
    * in the `cocoscr` encoding it's exceptionally two bytes: `{xd0}`
    * in the `utf16be` and `utf16le` encodings it's exceptionally two bytes: `{x00}{x00}`
    * in other encodings it's `{x00}` (this may be a subject to change in future versions).

##### Available only in some encodings

* `{apos}` – apostrophe/single quote (available everywhere except for `zx80`, `zx81` and `galaksija`)

* `{q}` – double quote symbol (available everywhere except for `pokemon1*` encodings)

* `{n}` – new line

* `{b}` – backspace

* `{lbrace}`, `{rbrace}` – opening and closing curly brace (only in encodings that support braces)

* `{up}`, `{down}`, `{left}`, `{right}` – control codes for moving the cursor

* `{white}`, `{black}`, `{red}`, `{green}`, `{blue}`, `{cyan}`, `{yellow}`, `{purple}` – 
control codes for changing the text color (`petscii`, `petsciijp`, `sinclair` only)

* `{bgwhite}`, `{bgblack}`, `{bgred}`, `{bggreen}`, `{bgblue}`, `{bgcyan}`, `{bgyellow}`, `{bgpurple}` – 
control codes for changing the text background color (`sinclair` only)

* `{reverse}`, `{reverseoff}` – inverted mode on/off

* `{yen}`, `{pound}`, `{cent}`, `{euro}`, `{copy}` – yen symbol, pound symbol, cent symbol, euro symbol, copyright symbol

* `{nbsp}`, `{shy}` – non-breaking space, soft hyphen

* `{pi}` – letter π

* `{u0000}`–`{u1fffff}` – Unicode codepoint (available in UTF encodings only)

##### Character availability

For ISO/DOS/Windows/UTF encodings, consult external sources.

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
`msx_intl`,`msx_br` | yes  | yes | ¢£¥  | Western   | yes   
`msx_jp`            | yes  | no  | ¥    | katakana  | yes   
`msx_ru`            | yes  | yes |      | Russian⁴  | yes   
`koi7n2`            | no   | yes |      | Russian⁵  | no   
`koi8*`             | yes  | yes |      | Russian   | no   
`cpc_en`            | yes  | yes | £    | none      | yes 
`cpc_es`            | yes  | yes |      | Spanish⁶  | yes 
`cpc_fr`            | yes  | no  | £    | French⁷   | yes 
`cpc_da`            | yes  | no  | £    | Nor/Dan.  | yes 
`vectrex`           | no   | yes |      | none      | no   
`coco`,`cocoscr`    | no   | yes |      | none      | no   
`pokemon1jp`        | no   | no  |      | both kana | no
`pokemon1en`        | yes  | no  |      | none      | no
`pokemon1fr`        | yes  | no  |      | Ger/Fre.  | no
`pokemon1es`        | yes  | no  |      | Spa/Ita.  | no
`galaksija`         | no   | no  |      | Yugoslav⁸ | no
  
1. `pet`, `origpet` and `petscr` cannot display card suit symbols and lowercase letters at the same time.
Card suit symbols are only available in graphics mode,
in which lowercase letters are displayed as uppercase and uppercase letters are displayed as symbols. 

2.  `oldpet` cannot display card suit symbols and lowercase letters at the same time.
Card suit symbols are only available in graphics mode, in which lowercase letters are displayed as symbols. 

3. `petjp` and `petscrjp` cannot display card suit symbols and katakana at the same time.
Card suit symbols are only available in graphics mode, in which katakana is displayed as symbols. 

4. Letter **Ё** and uppercase **Ъ** are not available.

5. Only uppercase. Letters **Ё** and **Ъ** are not available.

6. No accented vowels.

7. Some accented vowels are not available.

8. Letter **Đ** is not available.

If the encoding does not support lowercase letters (e.g. `apple2`, `petjp`, `petscrjp`, `koi7n2`, `vectrex`),
then text and character literals containing lowercase letters are automatically converted to uppercase. 
Only unaccented Latin and Cyrillic letters will be converted as such.
Accented Latin letters will not be converted and will fail to compile without `-flenient-encoding`.     
To detect if your default encoding does not support lowercase letters, test `'A' == 'a'`.

##### Escape sequence availability

The table below may be incomplete.

Encoding | new line | braces | backspace | cursor movement | text colour | reverse | background colour  
---------|----------|--------|-----------|-----------------|-------------|---------|------------------  
`pet`,`petjp`       | yes | no  | no  | yes | yes | yes | no  
`origpet`           | yes | no  | no  | yes | no  | yes | no  
`oldpet`            | yes | no  | no  | yes | no  | yes | no  
`petscr`, `petscrjp`| no  | no  | no  | no  | no  | no  | no  
`sinclair`          | yes | yes | no  | yes | yes | yes | yes  
`zx80`,`zx81`       | yes | no  | yes | yes | no  | no  | no  
`ascii`, `iso_*`    | yes | yes | yes | no  | no  | no  | no  
`iso8869_*`, `cp*`  | yes | yes | yes | no  | no  | no  | no  
`apple2`            | no  | yes | no  | no  | no  | no  | no  
`apple2`            | no  | no  | no  | no  | no  | no  | no  
`apple2e`           | no  | yes | no  | no  | no  | no  | no  
`apple2gs`          | no  | yes | no  | no  | no  | no  | no  
`atascii`           | yes | no  | yes | yes | no  | no  | no  
`atasciiscr`        | no  | no  | no  | no  | no  | no  | no  
`msx_*`             | yes | yes | yes | yes | no  | no  | no  
`koi7n2`            | yes | no  | yes | no  | no  | no  | no  
`koi8*`             | yes | yes | yes | no  | no  | no  | no  
`vectrex`           | no  | no  | no  | no  | no  | no  | no  
`coco`              | yes | no  | yes | no  | no  | no  | no  
`cocoscr`           | no  | no  | no  | no  | no  | no  | no  
`utf*`              | yes | yes | yes | no  | no  | no  | no  
all the rest        | yes | yes | no  | no  | no  | no  | no
