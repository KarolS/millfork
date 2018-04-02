# Target platforms

Currently, Millfork supports creating disk- or tape-based programs for Commodore, Apple and Atari 8-bit computers, 
but it may be expanded to support other 6502-based platforms in the future.

## Supported platforms

The following platforms are currently supported:

* `c64` – Commodore 64

* `c64_scpu` – Commodore 64 with SuperCPU in emulation mode

* `c64_scpu16` – Commodore 64 with SuperCPU in native, 16-bit mode (very buggy)

* `c16` – Commodore 16

* `plus4` – Commodore Plus/4

* `vic20` – Commodore VIC-20 without memory expansion

* `vic20_3k` – Commodore VIC-20 with 3K memory expansion

* `vic20_8k` – Commodore VIC-20 with 8K or 16K memory expansion

* `c128` – Commodore 128 in its native mode

* `pet` – Commodore PET

* `nes_small` – a tiny 32K PRGROM + 8K CHRROM Famicom/NES program, using iNES mapper 0 (NROM)

* `nes_mcc4` – a 128K PRGROM + 128K CHRROM + extra 8KRAM Famicom/NES program, using iNES mapper 10 (MMC4)  
For more complex programs, you need to create your own "platform" definition.  
Read [the NES programming guide](./famicom-programming-guide.md) for more info.

* `a8` – Atari 8-bit computers

* `bbcmicro` – BBC Micro model B (32k RAM)

* `apple2` – Apple II+/IIe/Enhanced IIe

The primary and most tested platform is Commodore 64.

Currently, targets that assume that the program will be loaded from disk or tape are better tested.
Cartridge targets may exhibit unexpected bugs.

### A note about Apple II

Apple II variants other than II+/IIe/Enhanced IIe are untested;
this includes the original II, IIc and IIc+, but also later compatible computers (Apple III and IIgs).
They may or may not work.

The compiler output is a raw machine code file, which then has to be put on a disk. 
You can do it using [CiderPress](http://a2ciderpress.com/), 
[AppleCommander](https://applecommander.github.io/), 
or some other tool.

The file has to be loaded from $0C00. An example how to put such file onto a disk using AppleCommander:

    java -jar AppleCommander-1.3.5.jar -p disk_image.dsk FILENAME B 0xc00 < compiler_output.a2
    
Creating a bootable disk is beyond the scope of this document.

### A note about BBC Micro

The default configuration file puts the start address for the program at $0E00.

The compiler outputs two files: a raw machine code file without an extension and a `.inf` file with file metadata.
To use the file, you need to put it on a disk or a disk image.
You can for example use tools like BBC Disk Explorer.

After putting it on a disk, the file can be run with:

    *RUN "FILENAME"
    
Currently, multipart BBC Micro programs are not supported.

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

* `modules` – comma-separated list of modules that will be automatically imported

* other compilation options (they can be overridden using commandline options):

    * `emit_illegals` – whether the compiler should emit illegal instructions, default `false`
    
    * `emit_cmos` – whether the compiler should emit CMOS instructions, default is `true` on compatible processors and `false` elsewhere

    * `emit_65816` – which 65816 instructions should the compiler emit, either `no`, `emulation` or `native` 
    
    * `decimal_mode` – whether the compiler should emit decimal instructions, default is `false` on `ricoh` and `strictricoh` and `true` elsewhere
    
    * `ro_arrays` – whether the compiler should warn upon array writes, default is `false`
    
    * `prevent_jmp_indirect_bug` – whether the compiler should try to avoid the indirect JMP bug, 
    default is `false` on 65C02-compatible processors and `true` elsewhere
    
    * `compact_dispatch_params` – whether parameter values in return dispatch statements may overlap other objects, default is `true`  
    This may cause problems if the parameter table is stored next to a hardware register that has side effects when reading.  


#### `[allocation]` section

* `zp_pointers` – either a list of comma separated zeropage addresses that can be used by the program as zeropage pointers, or `all` for all. Each value should be the address of the first of two free bytes in the zeropage.

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
    
    * `per_segment` – generate a separate file with each segment

* `format` – output file format; a comma-separated list of tokens:

    * literal byte values
    
    * `startaddr` – little-endian 16-bit address of the first used byte of the compiled output (not necessarily the segment start)
    
    * `endaddr` – little-endian 16-bit address of the last used byte of the compiled output (usually not the segment end)
    
    * `allocated` – all used bytes
    
    * `<addr>:<addr>` - inclusive range of bytes
    
    * `<segment>:<addr>:<addr>` - inclusive range of bytes in a given segment
    
* `extension` – target file extension, with or without the dot