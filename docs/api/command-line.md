[< back to index](../index.md)

# Command-line options

## General options

* `--version` – Display the version number and quit.

* `--help` – Displays help the command line option.

* `--` – End of the options, all the following parameters will be treated as input files, even if they look like options.

## I/O options

*  `-o <file>` – Output filename, without extension.
Extension will be added automatically,
`.prg` for Commodore (including LUnix/LNG),
`.a2` for Apple,
`.xex` for Atari computers,
`.bin` for Atari VCS,
`.nes` for NES,
no extension for BBC micro program file,
`.inf` for BBC Micro metadata,
`.d88` for PC-88 disk images,
`.tap` for ZX-Spectrum tape images.

* `-s` – Generate also the assembly output. It is not compatible with any assembler, but it serves purely informational purpose. The file has the same nam as the output file and the extension is `.asm`.

* `-g` – Generate also the label file. The label file contains labels with their addresses, with duplicates removed. It can be loaded into the monitor of the Vice emulator for debugging purposes. The file has the same name as the output file and the extension is `.lbl`.

* `-I  <dir>;<dir>` – The include directories. The current working directory is also an include directory. Those directories are searched for modules and platform definitions.
 
* `-t <platform>` – Target platform. It is loaded from an `.ini` file found in any of the include directories. See also [this document](target-platforms.md).

* `-r <program>` – Run given program after successful compilation. Useful for automatically launching emulators without any external scripting.

* `-D <feature>=<value>` – Defines a feature value for the preprocessor.

## Verbosity options

 * `-q` – Suppress all messages except for errors.

* `-v`, `-vv`, `-vvv` – Increase verbosity, various levels.

## Code generation options

* `-fcmos-ops`, `-fno-cmos-ops` – Whether should emit CMOS opcodes.  
`.ini` equivalent: `emit_cmos`.
Default: yes if targeting a 65C02-compatible architecture, no otherwise.

* `-fillegals`, `-fno-illegals` – Whether should emit illegal (undocumented) NMOS opcodes.  
`.ini` equivalent: `emit_illegals`.
Default: no.

* `-f65ce02-ops`, `-fno-65ce02-ops` – Whether should emit 65CE02 opcodes.  
`.ini` equivalent: `emit_65ce026`. 
Default: yes if targeting 65CE02, no otherwise.

* `-fhuc6280-ops`, `-fno-huc6280-ops` – Whether should emit HuC6280 opcodes.  
`.ini` equivalent: `emit_huc6280`. 
Default: yes if targeting HuC6280, no otherwise.

* `-fno-65816-ops`, `-femulation-65816-ops`, `-fnative-65816-ops` – Which subset of 65816 instructions to support. 
`-fnative-65816-ops` is required to use any 16-bit operations. 
Currently, there is not much support in the compiler for the native mode.
`.ini` equivalent: `emit_65816`. 
Default: native if targeting 65816, no otherwise.

* `-fjmp-fix`, `-fno-jmp-fix` – Whether should prevent indirect JMP bug on page boundary.  
`.ini` equivalent: `prevent_jmp_indirect_bug`.
Default: no if targeting a 65C02-compatible architecture or a non-6502 architecture, yes otherwise.

* `-fzp-register`, `-fno-zp-register` – Whether should reserve 2 bytes of zero page as a pseudoregister.
Increases language features. 
`.ini` equivalent: `zeropage_register`.
Default: yes if targeting a 6502-based architecture, no otherwise.
                    
* `-fdecimal-mode`, `-fno-decimal-mode` – Whether decimal mode should be available.  
`.ini` equivalent: `decimal_mode`.
Default: no if targeting Ricoh, yes otherwise.

* `-fvariable-overlap`, `-fno-variable-overlap` – Whether variables should overlap if their scopes do not intersect.  
Default: yes.

* `-fbounds-checking`, `-fno-bounds-checking` – Whether should insert bounds checking on array access.  
Default: no.

* `-fcompact-dispatch-params`, `-fno-compact-dispatch-params` – 
Whether parameter values in return dispatch statements may overlap other objects. 
This may cause problems if the parameter table is stored next to a hardware register that has side effects when reading.  
`.ini` equivalent: `compact_dispatch_params`. Default: yes.

* `-flenient-encoding`, `-fno-lenient-encoding` – 
Whether the compiler should allow for invalid characters in string/character literals that use the default encodings and replace them with alternatives.
.ini` equivalent: `lenient_encoding`. Default: no.

## Optimization options

* `-O0` – Disable all optimizations.

* `-O`, `-O2` ... `-O8` – Optimize code, various levels. For most code, anything above `-O4` doesn't improve it anymore. 

* `-O9` – Optimize code using superoptimizer (experimental). Computationally expensive, decent results.

* `-finline`, `-fno-inline` – Whether should inline functions automatically (experimental).
See the [documentation about inlining](../abi/inlining.md). Computationally easy, can give decent gains.
`.ini` equivalent: `inline`.

* `-fipo`, `-fno-ipo` – Whether should perform interprocedural optimization.
It enables certain optimization similar to what inlining would enable, but without actual inlining.
`.ini` equivalent: `ipo`.

* `-Os`, `--size` – Optimize for size, sacrificing some speed (experimental).

* `-Of`, `--fast` – Optimize for speed, even if it increases the size a bit (experimental).

* `-Ob`, `--blast-processing` – Optimize for speed, even if it increases the size a lot (experimental).
Enables `-finline` automatically. 

* `-fdangerous-optimizations` – Use dangerous optimizations (experimental).
Dangerous optimizations are more likely to result in broken code.
Enables `-fipo` automatically.
`.ini` equivalent: `dangerous_optimizations`. 

Note: for compatibility with versions 0.3.0 and earlier,
command line options `--inline`, `--dangerous-optimizations` `--fipo` and `--fno-ipo` are still recognized, but discouraged.

## Warning options

* `-Wall` – Enable extra warnings.

* `-Wfatal` – Treat warnings as errors.
