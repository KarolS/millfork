[< back to index](../doc_index.md)

# Text encodings ans escape sequences

### Text encoding list

* `default` – default console encoding (can be omitted)

* `scr` – default screencodes
(usually the same as `default`, a notable exception are the Commodore computers)

* `ascii` – standard ASCII

* `pet` or `petscii` – PETSCII (ASCII-like character set used by Commodore machines from VIC-20 onward)

* `origpet` or `origpetscii` – old PETSCII (Commodore PET with original ROMs)

* `oldpet` or `oldpetscii` – old PETSCII (Commodore PET with newer ROMs)

* `cbmscr` or `petscr` – Commodore screencodes

* `apple2` – Apple II charset ($A0–$CF)

* `bbc` – BBC Micro character set

* `sinclair` – ZX Spectrum character set

* `jis` or `jisx` – JIS X 0201

* `iso_de`, `iso_no`, `iso_se`, `iso_yu` – various variants of ISO/IEC-646
 
* `iso_dk`, `iso_fi` – aliases for `iso_no` and `iso_se` respectively
 
* `atascii` or `atari` – ATASCII as seen on Atari 8-bit computers
 
* `atasciiscr` or `atariscr` – screencodes used by Atari 8-bit computers

When programming for Commodore,
use `pet` for strings you're printing using standard I/O routines
and `petscr` for strings you're copying to screen memory directly.

### Escape sequences

##### Available everywhere

* `{q}` – double quote symbol

* `{apos}` – apostrophe/single quote

* `{x00}`–`{xff}` – a character of the given hexadecimal value

##### Available only in some encodings

* `{n}` – new line

* `{b}` – backspace

* `{lbrace}`, `{rbrace}` – opening and closing curly brace (only in encodings that support braces)

* `{up}`, `{down}`, `{left}`, `{right}` – control codes for moving the cursor

* `{white}`, `{black}`, `{red}`, `{green}`, `{blue}`, `{cyan}`, `{yellow}`, `{purple}` – 
control codes for changing the text color

* `{bgwhite}`, `{bgblack}`, `{bgred}`, `{bggreen}`, `{bgblue}`, `{bgcyan}`, `{bgyellow}`, `{bgpurple}` – 
control codes for changing the text background color

* `{reverse}`, `{reverseoff}` – inverted mode on/off

##### Escape sequence availability

Encoding | new line | braces | backspace | cursor movement | text colour, reverse | background colour  
--|--|--|--|--|--|--  
`pet`              | yes | no  | no  | yes | yes | no  
`petscr`           | no  | no  | no  | no  | no  | no  
`sinclair`         | yes | yes | no  | yes | yes | yes  
`ascii`, `iso_*`   | yes | yes | yes | no  | no  | no  
`apple2`           | no  | yes | no  | no  | no  | no  
`atascii`          | yes | no  | yes | yes | no  | no  
`atasciiscr`       | no  | no  | no  | no  | no  | no  
all the rest       | yes | yes | no  | no  | no  | no
