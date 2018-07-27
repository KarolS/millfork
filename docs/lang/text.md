[< back to index](../index.md)

# Text encodings ans escape sequences

### Text encoding list

* `default` – default console encoding (can be omitted)

* `scr` – default screencodes
(usually the same as `default`, a notable exception are the Commodore computers)

* `ascii` – standard ASCII

* `pet` or `petscii` – PETSCII (ASCII-like character set used by Commodore machines)

* `cbmscr` or `petscr` – Commodore screencodes

* `apple2` – Apple II charset ($A0–$FE)

* `bbc` – BBC Micro character set

* `sinclair` – ZX Spectrum character set

* `jis` or `jisx` – JIS X 0201

* `iso_de`, `iso_no`, `iso_se`, `iso_yu` – various variants of ISO/IEC-646
 
* `iso_dk`, `iso_fi` – aliases for `iso_no` and `iso_se` respectively

When programming for Commodore,
use `pet` for strings you're printing using standard I/O routines
and `petscr` for strings you're copying to screen memory directly.

### Escape sequences

##### Available everywhere

* `{n}` – new line

* `{q}` – double quote symbol

* `{apos}` – apostrophe/single quote

* `{x00}`–`{xff}` – a character of the given hexadecimal value

##### Available only in some encodings

* `{b}` – backspace

* `{lbrace}`, `{rbrace}` – opening and closing curly brace (only in encodings that support braces)

* `{up}`, `{down}`, `{left}`, `{right}` – control codes for moving the cursor

* `{white}`, `{black}`, `{red}`, `{green}`, `{blue}`, `{cyan}`, `{yellow}`, `{purple}` – 
control codes for changing the text color

* `{bgwhite}`, `{bgblack}`, `{bgred}`, `{bggreen}`, `{bgblue}`, `{bgcyan}`, `{bgyellow}`, `{bgpurple}` – 
control codes for changing the text background color

* `{reverse}`, `{reverseoff}` – inverted mode on/off

##### Escape sequence availability

Encoding | braces | backspace | cursor movement | text colour and reverse | background colour
--|--|--|--|--
`pet`              | no  | no  | yes | yes | no
`petscr`           | no  | no  | no  | no  | no
`sinclair`         | yes | no  | yes | yes | yes
`ascii`, `iso_*`   | yes | yes | no  | no  | no
all the rest       | yes | no  | no  | no  | no
