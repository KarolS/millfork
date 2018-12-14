[< back to index](../index.md)

## Adding a custom platform

Every platform is defined in an `.ini` file with an appropriate name.

#### `[compilation]` section

* `arch` – CPU architecture. It defines which instructions are available. Available values: 

    * `nmos` (original 6502)
    
    * `strict` (NMOS without illegal instructions) 
    
    * `ricoh` (Ricoh 2A03/2A07, NMOS without decimal mode) 
    
    * `strictricoh` (Ricoh 2A03/2A07 without illegal instructions)
    
    * `cmos` (WDC 65C02 or 65SC02)
    
    * `65ce02` (CSG 65CE02; experimental)
    
    * `huc6280` (Hudson HuC6280; experimental)
    
    * `65816` (WDC 65816/65802; experimental; currently only programs that use only 16-bit addressing are supported)
    
    * `z80` (Zilog Z80; experimental and slightly incomplete)
    
    * `i8080` (Intel 8080; experimental, buggy and very incomplete)
    
    * `gameboy` (Sharp LR35902; experimental, buggy and very incomplete)

* `encoding` – default encoding for console I/O, one of 
`ascii`, `pet`/`petscii`, `petscr`/`cbmscr`, `atascii`, `bbc`, `jis`/`jisx`, `apple2`,
`iso_de`, `iso_no`/`iso_dk`, `iso_se`/`iso_fi`, `iso_yu`. Default: `ascii`

* `screen_encoding` – default encoding for screencodes (literals with encoding specified as `scr`). 
Default: the same as `encoding`.

* `modules` – comma-separated list of modules that will be automatically imported

* other compilation options (they can be overridden using commandline options):

    * `emit_illegals` – whether the compiler should emit illegal instructions, default `false`
    
    * `emit_cmos` – whether the compiler should emit CMOS instructions, default is `true` on compatible processors and `false` elsewhere

    * `emit_65816` – which 65816 instructions should the compiler emit, either `no`, `emulation` or `native` 
    
    * `decimal_mode` – whether the compiler should emit decimal instructions, default is `false` on `ricoh` and `strictricoh` and `true` elsewhere;
    if disabled, a software decimal mode will be used
    
    * `ro_arrays` – whether the compiler should warn upon array writes, default is `false`
    
    * `prevent_jmp_indirect_bug` – whether the compiler should try to avoid the indirect JMP bug, 
    default is `false` on 65C02-compatible or non-6502 processors and `true` elsewhere
    
    * `compact_dispatch_params` – whether parameter values in return dispatch statements may overlap other objects, default is `true`  
    This may cause problems if the parameter table is stored next to a hardware register that has side effects when reading.
    
    * `lunix` – generate relocatable code for LUnix/LNG, default is `false`
    
    * `zeropage_register` – reserve a certain amount of bytes of zero page as a pseudoregister to increase language features.
    Default: `4` if targeting a 6502-based architecture, `0` otherwise.  
    `true` is a synonym of the current compiler default (currently: 4) and `false` is a synonym for 0.
    
    * `inline` - inline functions automatically by default, default is `false`.
    
    * `ipo` - enable interprocedural optimization, default is `false`.
    
    * `lenient_encoding` - allow for automatic substitution of invalid characters in string literals using the default encodings, default is `false`.
    
    * `use_shadow_registers_for_irq` – use Z80 shadow registers in interrupt routines, default is `true` for Z80 and `false` otherwise
    
    * `ix_stack` – use the IX register to access stack variables, default is `true` for Z80 and `false` otherwise
    
    * `iy_stack` – use the IY register to access stack variables, default is `false`
    
    * `ix_scratch` – allow using the IY register for other purposes, default is `false`
    
    * `iy_scratch` – allow using the IY register for other purposes, default is `false`
    
    * `software_stach` – use software stack for stack variables, default is `false`
    
    * `output_intel_syntax` – use Intel syntax instead of Zilog syntax, default is `true` for Intel 8080 and `false` otherwise


#### `[define]` section

This section defines values of features of the target.
See the [preprocessor documentation](../lang/preprocessor.md) for more info.

#### `[allocation]` section

* `zp_pointers` – 
either a list of comma separated zeropage addresses that can be used by the program as zeropage pointers, or `all` for all. 
Each value should be the address of the first of two free bytes in the zeropage.
Only used for 6502-based targets.

* `segments` – a comma-separated list of segment names.  
A segment named `default` is always required.  
Default: `default`. In all options below, `NAME` refers to a segment name.

* `default_code_segment` – the default segment for code and initialized arrays.  
Note that the default segment for uninitialized arrays and variables is always `default`.  
Default: `default`

* `segment_NAME_start` – the first address used for automatic allocation in the segment.  
Note that the `default` segment shouldn't start before $200, as the $0-$1FF range is reserved for the zeropage and the stack.  
The `main` function will be placed as close to the beginning of its segment as possible, but not necessarily at `segment_NAME_start`

* `segment_NAME_end` – the last address in the segment

* `segment_NAME_codeend` – the last address in the segment for code and initialized arrays.  
Only uninitialized variables are allowed between `segment_NAME_codeend` and `segment_NAME_end`.  
Default: the same as `segment_NAME_end`.

* `segment_NAME_datastart` – the first address used for non-zeropage variables, or `after_code` if the variables should be allocated after the code.  
Default: `after_code`.

#### `[output]` section
 
* `style` – how multi-segment programs should be output:

    * `single` – output a single file, based mostly, but not necessarily only on data in the `default` segment (the default)
    
    * `lunix` – like `single`, but add data necessary for relocation between code and data (requires `lunix` option in the `compilation` section)
    
    * `per_segment` – generate a separate file with each segment

* `format` – output file format; a comma-separated list of tokens:

    * literal byte values
    
    * `startaddr` – little-endian 16-bit address of the first used byte of the compiled output (not necessarily the segment start)
    
    * `startpage` – the high byte of `startaddr`
    
    * `endaddr` – little-endian 16-bit address of the last used byte of the compiled output (usually not the segment end)
    
    * `allocated` – all used bytes
    
    * `pagecount` – the number of pages used by all used bytes (including partially filled pages)
    
    * `<addr>:<addr>` - inclusive range of bytes
    
    * `<segment>:<addr>:<addr>` - inclusive range of bytes in a given segment
    
    * `d88` - a D88 floppy disk image for PC-88
    
    * `tap` - a tape disk image for ZX Spectrum
    
* `extension` – target file extension, with or without the dot

* `bbc_inf` – should the `.inf` file with file metadata for BBC Micro be created
