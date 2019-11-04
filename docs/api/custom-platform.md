[< back to index](../doc_index.md)

## Adding a custom platform

Every platform is defined in an `.ini` file with an appropriate name.

As an extension, multiline entries are supported:
if a line ends with a backslash character, the value continues to the next line. 

#### `[compilation]` section

* `arch` – CPU architecture. It defines which instructions are available. Available values: 

    * `nmos` (original 6502)
    
    * `strict` (NMOS without illegal instructions) 
    
    * `ricoh` (Ricoh 2A03/2A07, NMOS without decimal mode) 
    
    * `strictricoh` (Ricoh 2A03/2A07 without illegal instructions)
    
    * `cmos` (65SC02, or any 65C02 without bit instructions)
    
    * `65sc02` (65SC02)
    
    * `65c02` (Rockwell 65C02; the instructions `BBRn`, `BBSn`, `SMBn`, `RMBn` are not supported yet)
    
    * `w65c02` (WDC 65C02)
    
    * `65ce02` (CSG 65CE02; experimental)
    
    * `huc6280` (Hudson HuC6280; experimental, many instructions are not supported yet)
    
    * `65816` (WDC 65816/65802; experimental; currently only programs that use only 16-bit addressing are supported)
    
    * `z80` (Zilog Z80)
    
    * `strictz80` (Z80 without illegal instructions)
    
    * `z80next` (Z80 core from ZX Spectrum Next)
    
    * `i8080` (Intel 8080)
    
    * `i8085` (Intel 8085)
    
    * `strict8085` (Intel 8085 without illegal instructions)
    
    * `gameboy` (Sharp LR35902; experimental)
    
    * `i8086` (Intel 8086; very experimental, very buggy and very, very incomplete –
    see the [8086 support disclaimer](../lang/x86disclaimer.md))

    * `6809` (Motorola 6809; very experimental, very buggy and very, very incomplete –
    many language features simply do not work at all for this target)

* `encoding` – default encoding for console I/O. Default: `ascii`.
See [the list of available encodings](../lang/text.md).

* `screen_encoding` – default encoding for screencodes (literals with encoding specified as `scr`). 
Default: the same as `encoding`.

* `modules` – comma-separated list of modules that will be automatically imported

* other compilation options (they can be overridden using commandline options):

    * `emit_illegals` – whether the compiler should emit illegal instructions, default `false`
    
    * `emit_cmos` – whether the compiler should emit 65C02 instructions, default is `true` on compatible processors and `false` elsewhere

    * `emit_65816` – which 65816 instructions should the compiler emit, either `no`, `emulation` or `native` 
    
    * `decimal_mode` – whether the compiler should emit decimal instructions, default is `false` on `ricoh` and `strictricoh` and `true` elsewhere;
    if disabled, a software decimal mode will be used

    * `emit_8085` – whether the compiler should emit Intel 8085 instructions, default is `true` on compatible processors and `false` elsewhere

    * `emit_x80` – whether the compiler should emit instructions present on Sharp LR35902 and Z80, but absent on Intel 8080, default is `true` on compatible processors and `false` elsewhere

    * `emit_z80` – whether the compiler should emit Zilog Z80 instructions not covered by `emit_x80`, default is `true` on compatible processors and `false` elsewhere
    
    * `prevent_jmp_indirect_bug` – whether the compiler should try to avoid the indirect JMP bug, 
    default is `false` on 65C02-compatible or non-6502 processors and `true` elsewhere
    
    * `compact_dispatch_params` – whether parameter values in return dispatch statements may overlap other objects, default is `true`.  
    This may cause problems if the parameter table is stored next to a hardware register that has side effects when reading.
    
    * `lunix` – generate relocatable code for LUnix/LNG, default is `false`
    
    * `zeropage_register` – reserve a certain amount of bytes of zero page as a pseudoregister to increase language features.
    Default: `4` if targeting a 6502-based architecture, `0` otherwise.  
    `true` is a synonym of the current compiler default (currently: 4) and `false` is a synonym for 0.
    
    * `inline` - inline functions automatically by default, default is `false`.
    
    * `ipo` - enable interprocedural optimization, default is `false`.
    
    * `function_fallthrough` – whether should replace a tail call by simply putting one function after another, default is `true`.

    * `function_deduplication` – whether identical functions should be merged into one function, default is `true`.

    * `subroutine_extraction` – whether identical fragments of functions should be extracted into subroutines, default is `false`.    
    
    * `lenient_encoding` - allow for automatic substitution of invalid characters in string literals using the default encodings, default is `false`.
    
    * `use_shadow_registers_for_irq` – use Z80 shadow registers in interrupt routines, default is `true` for Z80 and `false` otherwise
    
    * `ix_stack` – use the IX register to access stack variables, default is `true` for Z80 and 8086, `false` otherwise
    
    * `iy_stack` – use the IY register to access stack variables, default is `false`
    
    * `ix_scratch` – allow using the IY register for other purposes, default is `false`
    
    * `iy_scratch` – allow using the IY register for other purposes, default is `false`
    
    * `software_stack` – use software stack for stack variables, default is `false`. Applicable only to 6502-based targets.
    
    * `output_intel_syntax` – use Intel syntax instead of Zilog syntax, default is `true` for Intel 8080/8085 and `false` otherwise


#### `[define]` section

This section defines values of features of the target.
See the [preprocessor documentation](../lang/preprocessor.md) for more info.

#### `[allocation]` section

* `zp_pointers` – 
either a list of comma separated zeropage addresses that can be used by the program as zeropage pointers, or `all` for all. 
Each value should be the address of the first of two free bytes in the zeropage.
Only used for 6502-based targets. Cannot be used together with `zp_bytes`.

* `zp_bytes` – 
either a list of comma separated zeropage byte addresses or address ranges that can be used by the program in zeropage, or `all` for all. 
Only used for 6502-based targets. Cannot be used together with `zp_pointers`.

* `segments` – a comma-separated list of segment names.  
A segment named `default` is always required.  
Default: `default`. In all options below, `NAME` refers to a segment name.

* `default_code_segment` – the default segment for code and const arrays.  
Note that the default segment for writable arrays and variables is always `default`.  
Default: `default`

* `ram_init_segment` – the segment storing a copy of initial values for preinitialized writable arrays and variables.
The segment cannot be `default`. See [the ROM vs RAM guide](./rom-vs-ram.md) for more information.
Default: none.

* `segment_NAME_start` – the first address used for automatic allocation in the segment.  
Note that on 6502-like targets, the `default` segment shouldn't start before $200, as the $0-$1FF range is reserved for the zeropage and the stack.  
The first object defined in `segment_NAME_layout` (usually the `main` function)
will be placed as close to the beginning of its segment as possible,
but not necessarily at `segment_NAME_start`

* `segment_NAME_end` – the last address in the segment

* `segment_NAME_codeend` – the last address in the segment for code and const arrays.  
Only uninitialized variables are allowed between `segment_NAME_codeend` and `segment_NAME_end`.  
Default: the same as `segment_NAME_end`.

* `segment_NAME_datastart` – the first address used for non-zeropage variables, or `after_code` if the variables should be allocated after the code.  
Default: `after_code`.

* `segment_NAME_bank` – the bank number the segment belongs to. Default: `0`.
For better debugging on NES, RAM segments should use bank number `$ff`.

* `segment_NAME_fill` – the byte value used to fill gaps and other unused space in the bank. Default: `0`.

* `segment_NAME_layout` – a comma-separated list of object names that defines in what order the objects are laid out in the segment.
One item has to be `*`, it means "all the other objects".  
For example, `a,b,*,c,d` means that the output will contain `a` first, then `b`, then everything else except for `c` and `d`,
then `c` and finally `d`.
If an object from that list does not exist, it is ignored.  
Default: `main,*`

#### `[output]` section
 
* `style` – how multi-segment programs should be output:

    * `single` – output a single file, based mostly, but not necessarily only on data in the `default` segment (the default)
    
    * `lunix` – like `single`, but add data necessary for relocation between code and data (requires `lunix` option in the `compilation` section)
    
    * `per_segment` – generate a separate file with each segment

* `format` – output file format; a comma-separated list of tokens:

    * literal byte values
    
    * `startaddr` – little-endian 16-bit address of the first used byte of the compiled output (not necessarily the segment start)
    
    * `startaddr_be` – the same, but big-endian
    
    * `endaddr` – little-endian 16-bit address of the last used byte of the compiled output (usually not the segment end)
    
    * `endaddr_be` – the same, but big-endian
    
    * `startaddr+123`, `startaddr_be+123`, `endaddr+123`, `endaddr_be+123` – the same, but incremented by the given number

    * `startaddr-123`, `startaddr_be-123`, `endaddr-123`, `endaddr_be-123` – the same, but decremented by the given number
    
    * `startpage` – the high byte of `startaddr`
    
    * `length` – little-endian 16-bit length of the compiled output; `endaddr - startaddr + 1`
    
    * `length_be` – the same, but big-endian
    
    * `length+123`, `length_be+123` – the same, but incremented by the given number
    
    * `length-123`, `length_be-123` – the same, but decremented by the given number

    * `allocated` – all used bytes
    
    * `pagecount` – the number of pages used by all used bytes (including partially filled pages)
    
    * `<addr>:<addr>` - inclusive range of bytes
    
    * `<segment>:<addr>:<addr>` - inclusive range of bytes in a given segment
    
    * `d88` - a D88 floppy disk image for PC-88
    
    * `tap` - a tape disk image for ZX Spectrum
    
* `extension` – target file extension, with or without the dot

* `bbc_inf` – should the `.inf` file with file metadata for BBC Micro be created

* `gb_checksum` – should the main output file be patched with Game Boy-compatible checksums

* `labels` – format of the label file:

    * `vice` (the default) – format compatible with the Vice emulator. The extension is `.lbl`.
    
    * `nesasm` – format used by the NESASM assembler. The extension is `.fns`.
    
    * `sym` – format used by the WLA/DX assembler. The extension is `.sym`.
    
    * `fceux` – multi-file format used by the FCEUX emulator. The extension is `.nl`.
